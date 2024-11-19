{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Untyped.Eval (module Untyped.Eval) where
import Untyped.Types

import Data.List (nub)
import Data.Tree (Tree (..))

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

freevars :: Term -> [Variable]
freevars (Var x) = [x]
freevars (App p q) = nub (freevars p ++ freevars q)
freevars (Lam v p) = filter (/= v) (freevars p)

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

-- Perform reduction on all possible redexes (redexes are `App`s)
-- Recursive because there might be some `App` within terms.
oneStepReduction :: Term -> [Term]
oneStepReduction (Var _) = []
oneStepReduction (Lam v q) = [Lam v q' | q' <- oneStepReduction q]
oneStepReduction (App (Lam v p) q) = subst v q p : [App l q | l <- oneStepReduction (Lam v p)] ++ [App (Lam v p) q' | q' <- oneStepReduction q]
oneStepReduction (App p q) = [App p' q | p' <- oneStepReduction p] ++ [App p q' | q' <- oneStepReduction q]

-- Catches cyclic non-termination
completeReduction :: Term -> Tree Term
completeReduction t = go t []
  where
    go t history =
      let nextReductions = oneStepReduction t
          uniqueReductions = nub nextReductions
          -- Only continue paths that we haven't seen (or else there's a cycle!)
          validReductions = filter (`notElem` history) uniqueReductions
          -- Add current term to history for recursive calls
          newHistory = t : history
       in Node t [go r newHistory | r <- validReductions]

multiStepReduction :: Int -> Term -> Tree Term
multiStepReduction n t = go n t []
  where
    go 0 t _ = Node t []
    go n t history =
      let nextReductions = oneStepReduction t
          uniqueReductions = nub nextReductions
          -- Only continue paths that we haven't seen (or else there's a cycle!)
          validReductions = filter (`notElem` history) uniqueReductions
          -- Add current term to history for recursive calls
          newHistory = t : history
       in Node t [go (n - 1) r newHistory | r <- validReductions]

-- Pretty printing with cycle detection
prettyPrintTree :: Tree Term -> String
prettyPrintTree = go 0 []
  where
    go indent history node =
      let currentTerm = rootLabel node
          isCycle = currentTerm `elem` history
          newHistory = currentTerm : history
       in replicate indent ' '
            ++ show currentTerm
            ++ (if isCycle then " (cycle detected)" else "")
            ++ "\n"
            ++ concatMap (go (indent + 1) newHistory) (subForest node)