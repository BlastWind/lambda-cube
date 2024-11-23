{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Untyped.Eval (reduce) where
import Untyped.Types

import Data.List (nub)
import Data.Tree (Tree (..))

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

-- Lazy tree, potentially divergent
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

-- If node N ∈ ancestors(N), remove N
withoutCycles :: Eq a => Tree a -> Tree a
withoutCycles = go []
  where
    go :: Eq a => [a] -> Tree a -> Tree a
    go history (Node root children) =
      let isCycle = root `elem` history
          newHistory = root : history
       in if isCycle 
          then Node root []
          else Node root (map (go newHistory) children)

getLeafNodes :: Tree a -> [a]
getLeafNodes (Node x []) = [x]
getLeafNodes (Node _ ts) = concatMap getLeafNodes ts

-- Attempt to reduce a lambda expression to its beta normal form, lazily!
-- There are 3 outcomes:
-- 1) You get a beta normal form
-- 2) You get a weakly normal form because your expression does not have a beta normal form
-- 3) The term diverges, so your computation never halts
reduce :: Term -> Term
reduce term = 
    -- Final reductions. `completeReduction` is lazy.
    let reductionPaths = getLeafNodes (withoutCycles (completeReduction term))
    in if null reductionPaths 
       then term
       -- According to Church-Rosser, if there is a normal form, any path will get us there
       else head reductionPaths