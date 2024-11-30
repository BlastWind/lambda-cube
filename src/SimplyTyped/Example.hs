{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module SimplyTyped.Example (module SimplyTyped.Example) where
import SimplyTyped.Types
import Data.Either (fromRight)
import SimplyTyped.ReadPrint (parseTerm)


ex1_str :: String
ex1_str = "λx:a->b. λy:a. x y"

ex1 :: Term
ex1 = fromRight (Var "error") (parseTerm ex1_str)

oldvar1_str :: String
oldvar1_str = "x"

oldvar1 :: Term
oldvar1 = fromRight (Var "error") (parseTerm oldvar1_str)

oldvar2_str :: String
oldvar2_str = "λx:a.y"

oldvar2 :: Term
oldvar2 = fromRight (Var "error") (parseTerm oldvar2_str)

oldex1_str :: String
oldex1_str = "(λx:a. (λy:b. (y x) a) z) v"

oldex1 :: Term
oldex1 = fromRight (Var "error") (parseTerm oldex1_str)

oldomega_str :: String
oldomega_str = "(λx:a. x x)(λx:a. x x)"

oldomega :: Term
oldomega = fromRight (Var "error") (parseTerm oldomega_str)

oldselfApplication_str :: String
oldselfApplication_str = "(λx:a. (x x) x)(λx:a. (x x) x)"

oldselfApplication :: Term
oldselfApplication = fromRight (Var "error") (parseTerm oldselfApplication_str)

oldex2_str :: String
oldex2_str = "(λu:a. v)((λx:a. x x)(λx:a. x x))"

oldex2 :: Term
oldex2 = fromRight (Var "error") (parseTerm oldex2_str)

oldchurchNine_str :: String
oldchurchNine_str = "(λm.λn.λf.m (n f)) (λf.λx.f (f (f x))) (λf.λx.f ( f (f x)))"
  where 
    mult = "(λn.λf.λx.f (n f x))"
    three = "(λf.λx.f(f(f x)))"

oldchurchNine :: Term
oldchurchNine = fromRight (Var "error") (parseTerm oldchurchNine_str)



