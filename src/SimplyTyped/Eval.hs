{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module SimplyTyped.Eval (reduce) where
import SimplyTyped.Types

import Data.List (nub)
import Data.Tree (Tree (..))
import SimplyTyped.ReadPrint (parseTerm)
import SimplyTyped.TypeCheck (typ)
import Text.Megaparsec (errorBundlePretty)
import Data.Bifunctor (first)
import qualified Data.Map as Map

(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
x ∈ xs = x `elem` xs

(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
x ∉ xs = not (x ∈ xs)

freevars :: Term -> [Variable]
freevars (Var x) = [x]
freevars (App p q) = nub (freevars p ++ freevars q)
freevars (Lam v _ p) = filter (/= v) (freevars p)

subst :: Variable -> Term -> Term -> Term
subst v t (Var v') = if v == v' then t else Var v'
subst v t (App p q) = App (subst v t p) (subst v t q)
-- Raw substitution might lead to unexpected binds, so we rename when that happens
subst v t (Lam v' typ p)
  -- Think about this! If the variable to substitute is the same as the lambda var, we can just return unchanged!
  | v == v' = Lam v' typ p
  -- if blind substituion will unintentionally bind something in the `t` term,
  -- then we need to rename v' to something not in FV(t)
  | v' ∈ fv_t = Lam v'' typ (subst v'' t (subst v' (Var v'') p))
  | otherwise = Lam v' typ (subst v t p)
  where
    fv_t = freevars t
    -- keep adding apostrophies
    v'' = until (∉ fv_t) (++ "\'") v'

-- Perform reduction on all possible redexes (redexes are `App`s)
-- Recursive because there might be some `App` within terms.
oneStepReduction :: ProcessedTerm TypeChecked -> [ProcessedTerm TypeChecked]
oneStepReduction t@(TypeCheckedTerm term typ src) =
    case term of
        Var _ -> []
        Lam v ty q -> [TypeCheckedTerm (Lam v ty q') typ src | q' <- oneStepReduction' q]
        App (Lam v ty p) q ->
            TypeCheckedTerm (subst v q p) typ src : [TypeCheckedTerm (App (Lam v ty p) q') typ src | q' <- oneStepReduction' q]
        App p q ->
            [TypeCheckedTerm (App p' q) typ src | p' <- oneStepReduction' p] ++
            [TypeCheckedTerm (App p q') typ src | q' <- oneStepReduction' q]
    where
        oneStepReduction' = map getTerm . oneStepReduction . (\t -> TypeCheckedTerm t typ src)

-- Lazy tree, potentially divergent
completeReduction :: ProcessedTerm TypeChecked -> Tree (ProcessedTerm TypeChecked)
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

getLeafNodes :: Tree a -> [a]
getLeafNodes (Node x []) = [x]
getLeafNodes (Node _ ts) = concatMap getLeafNodes ts

reduce :: ProcessedTerm TypeChecked -> ProcessedTerm Normalized
reduce t = NormalizedTerm (getTerm normalForm) t
  where
    reductionTree = completeReduction t
    normalForms = getLeafNodes reductionTree
    -- Since STLC is strongly normalizing, there must be exactly one normal form
    normalForm = head normalForms
