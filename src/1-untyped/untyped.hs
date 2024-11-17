{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use camelCase" #-}
module Untyped (module Untyped) where

import Data.Foldable (fold)
import Text.Parsec
import Text.Parsec.String
import Data.List (nub)
import Data.Either (fromRight)

type Variable = String

-- A Lambda Term
data Term
  = -- Plain variables are valid lambda terms
    Var Variable
  | -- Notice that something that don't make sense like `(App (Var x) (Var x))` can be constructed and evaluated
    -- This is a flaw of the untyped lambda calculus
    -- For the later typed calculus, I don't think I will make it impossible to write `(App (Var x) (Var x))`,
    -- I will just make the validity of terms connected to their type validity
    App Term Term
  | -- A lambda abstraction/function
    Lam Variable Term
  deriving (Eq, Show)

data EvalResult
  = -- BetaNormal: reduced to something irreducible, early return
    BetaNormal Term
  | -- WeaklyNormal: reduced to a term that is irreducible
    WeaklyNormal Term
  | -- Divergent: all reduction paths gives the same term back
    Divergent Term

(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
x ∈ xs = x `elem` xs

(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
x ∉ xs = not (x ∈ xs)

deleteAll :: (Eq a) => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll a (x : xs) = if a == x then xs else x : deleteAll a xs

removeDups :: (Eq a) => [a] -> [a]
removeDups [] = []
removeDups (a : as) = a : go [a] as
  where
    go prevSeen (x : xs) =
      if x ∈ prevSeen
        then go prevSeen xs
        else x : go (x : prevSeen) xs
    go _ [] = []

freevars :: Term -> [Variable]
freevars (Var x) = [x]
freevars (App p q) = removeDups (freevars p ++ freevars q)
freevars (Lam v p) = deleteAll v (freevars p)

subst :: Variable -> Term -> Term -> Term
subst v t (Var v') = if v == v' then t else Var v'
subst v t (App p q) = App (subst v t p) (subst v t q)
-- Raw substitution might lead to unexpected binds, so we rename when that happens
subst v t (Lam v' p)
  -- Think about this! If the variable to substitute is the same as the lambda var, we can just return unchanged!
  | v == v' = Lam v' p
  -- if blind substituion will unintentionally bind something in the `t` term,
  -- then we need to rename v' to something not in FV(t)
  | v' ∈ fv_t = Lam v'' (subst v'' t (subst v' (Var v'') p))
  | otherwise = Lam v' (subst v t p)
  where
    fv_t = freevars t
    -- keep adding apostrophies
    v'' = until (∉ fv_t) (++ "\'") v'

-- Every `App` is a potential redex to reduce, therefore, the result is an array.
-- When encountering complex terms, we also recursively invoke `oneStepBetaReduction` because there might be some `App` within.
oneStepBetaReduction :: Term -> [Term]
oneStepBetaReduction (Var v) = []
oneStepBetaReduction (Lam v q) = [Lam v q' | q' <- oneStepBetaReduction q]
oneStepBetaReduction (App (Lam v p) q) =
  subst v q p : [App l q | l <- oneStepBetaReduction (Lam v p)] ++ [App (Lam v p) q' | q' <- oneStepBetaReduction q]
oneStepBetaReduction (App p q) = [App p' q | p' <- oneStepBetaReduction p] ++ [App p q' | q' <- oneStepBetaReduction q]

data ReductionNode = ReductionNode 
    { nodeTerm :: Term
    , reductions :: [(Term, ReductionNode)]
    } deriving (Show)

buildReductionTree :: Term -> ReductionNode
buildReductionTree t = buildReductionTreeWithHistory t []

buildReductionTreeWithHistory :: Term -> [Term] -> ReductionNode
buildReductionTreeWithHistory t history =
    let nextReductions = oneStepBetaReduction t
        uniqueReductions = nub nextReductions
        -- Only continue paths that don't result in previously seen terms
        validReductions = filter (\t' -> not $ t' `elem` history) uniqueReductions
        -- Add current term to history for recursive calls
        newHistory = t : history
        -- Build subtrees for valid reductions
        subTrees = [(t', buildReductionTreeWithHistory t' newHistory) 
                  | t' <- validReductions]
    in ReductionNode t subTrees

-- Pretty printing with cycle detection
prettyPrintTree :: ReductionNode -> String
prettyPrintTree = go 0 []
  where
    go indent history node = 
        let currentTerm = nodeTerm node
            isCycle = currentTerm `elem` history
            newHistory = currentTerm : history
        in replicate indent ' ' ++ 
           show currentTerm ++ 
           (if isCycle then " (cycle detected)" else "") ++
           "\n" ++
           concatMap (\(_, child) -> go (indent + 2) newHistory child) (reductions node)


omega_s :: String 
omega_s = "(λx. x x)(λx. x x)"

omega :: Term
omega = fromRight (Var "error") (parseLambda omega_s)

ex1_s :: String
ex1_s = "(λx. (λy. (y x) a) z) v"

ex1 :: Term
ex1 = fromRight (Var "error") (parseLambda ex1_s)

ex2_s :: String
ex2_s = "(λx. x x x)(λx. x x x)"

ex2 :: Term
ex2 = fromRight (Var "error") (parseLambda ex2_s)

ex3_s :: String
ex3_s = "(λu. v)((λx. x x)(λx. x x))"

ex3 :: Term
ex3 = fromRight (Var "error") (parseLambda ex3_s)



lamIdentifier :: Parser String
lamIdentifier = do
  first <- oneOf ['a' .. 'z']
  rest <- many (alphaNum <|> char '_' <|> char '\'')
  return (first : rest)

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  r <- p
  _ <- char ')'
  return r

whitespaces :: Parser ()
whitespaces = do
  _ <- many $ char '\t' <|> char ' '
  return ()


term :: Parser Term
term = do
  -- First parse a base term (abstraction, variable, or parenthesized term)
  t1 <- baseTerm
  -- Then look for optional applications
  applications t1
  where
    baseTerm =
      choice
        [ try abstraction,
          try (parens term),
          variable
        ]
    
    -- Handle zero or more applications
    applications :: Term -> Parser Term
    applications t1 = do
      whitespaces
      option t1 $ do
        t2 <- baseTerm
        applications (App t1 t2)

variable :: Parser Term
variable = Var <$> lamIdentifier

abstraction :: Parser Term
abstraction = do
  _ <- char '\\' <|> char 'λ'
  v <- lamIdentifier
  _ <- char '.'
  whitespaces
  t <- term
  return $ Lam v t

parseLambda :: String -> Either ParseError Term
parseLambda = parse (term <* eof) ""

prettyPrint :: Term -> String
prettyPrint (Var x) = x
prettyPrint (App p q) = "(" ++ prettyPrint p ++ prettyPrint q ++ ")"
prettyPrint (Lam v p) = "(λ" ++ v ++ "." ++ " " ++ prettyPrint p ++ ")"