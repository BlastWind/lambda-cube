module Untyped.Types(module Untyped.Types) where

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