module SimplyTyped.Types (module SimplyTyped.Types) where

type Variable = String

type TypeVariable = String

-- Examples:
--   a is TypeVar "a"
--   a -> b is FunctionType (TypeVar "a") (TypeVar "b")
--   a -> (b -> c) is FunctionType (TypeVar "a") (FunctionType (TypeVar "b") (TypeVar "c"))
--   (a -> b) -> (c -> d) is FunctionType (FunctionType (TypeVar "a") (TypeVar "b")) (FunctionType (TypeVar "c") (TypeVar "d"))
data Type
  = TypeVar TypeVariable
  | FunctionType Type Type -- right associative
  deriving (Eq, Show)


data Term
  = Var Variable
  | -- While no `(App (Var x) (Var x))` should be representable in STLC, we allow its construction
    -- It will just get rejected after type checking.
    App Term Term 
  | Lam Variable Type Term -- Note that variables have types in STLC!
  deriving (Eq, Show)

data Phase 
  = Unparsed String
  | Parsed Term
  | TypeChecked Term