{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
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

data Parsed
data TypeChecked
data Normalized

-- To be serious about type safety, we shouldn't export this.
-- Instead, make `parse`, `typ`, and `reduce` the sole and smart constructors.
data ProcessedTerm phase where
    ParsedTerm :: Term -> ProcessedTerm Parsed
    TypeCheckedTerm :: ProcessedTerm Parsed -> ProcessedTerm TypeChecked
    NormalizedTerm :: ProcessedTerm TypeChecked -> ProcessedTerm Normalized
deriving instance Eq (ProcessedTerm phase)
deriving instance Show (ProcessedTerm phase)

-- Now getting the term is simple for any phase
getTerm :: ProcessedTerm phase -> Term
getTerm (ParsedTerm t) = t
getTerm (TypeCheckedTerm t _ _) = t
getTerm (NormalizedTerm t _) = t