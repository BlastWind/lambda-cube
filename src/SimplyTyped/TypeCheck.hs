module SimplyTyped.TypeCheck where

import Data.Map (Map, insert, lookup)
import SimplyTyped.Types

type Context = Map Variable Type

typ :: Term -> Context -> Either String Type
typ (Var v) context =
  maybe
    (Left "Var is not typed in this context")
    Right
    (Data.Map.lookup v context)
typ (Lam v vTyp subterm) context = case typ subterm (Data.Map.insert v vTyp context) of
  Left a -> Left a
  Right subtermTyp -> Right $ FunctionType vTyp subtermTyp
typ (App term1 term2) context = do
  typ1 <- typ term1 context
  typ2 <- typ term2 context
  case typ1 of
    TypeVar _ -> Left "gone"
    FunctionType a b -> if a == typ2 then Right b else Left "gone"

