module Untyped (module Untyped) where

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
  deriving (Show)

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
oneStepBetaReduction (Var _) = []
oneStepBetaReduction (Lam v q) = [Lam v q' | q' <- oneStepBetaReduction q]
oneStepBetaReduction (App (Lam v p) q) =
  subst v q p : [App l q | l <- oneStepBetaReduction (Lam v p)] ++ [App (Lam v p) q' | q' <- oneStepBetaReduction q]
oneStepBetaReduction (App p q) = [App p' q | p' <- oneStepBetaReduction p] ++ [App p q' | q' <- oneStepBetaReduction q]

omega :: Term
omega =
  App
    (Lam "x" (App (Var "x") (Var "x")))
    (Lam "x" (App (Var "x") (Var "x")))

ex1 :: Term
ex1 =
  App
    ( Lam
        "x"
        ( App
            ( Lam
                "y"
                (App (Var "y") (Var "x"))
            )
            (Var "z")
        )
    )
    (Var "v")
    
ex1_s :: String
ex1_s = "(λx. (λy. yx) z) v"

-- TODO: Parsing. string <-> Term