{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Untyped.Example (module Untyped.Example) where
import Untyped.Types
import Data.Either (fromRight)
import Untyped.ReadPrint (parseTerm)

var1_str :: String
var1_str = "x"

var1 :: Term
var1 = fromRight (Var "error") (parseTerm var1_str)

var2_str :: String
var2_str = "λx.y"

var2 :: Term
var2 = fromRight (Var "error") (parseTerm var2_str)

ex1_str :: String
ex1_str = "(λx. (λy. (y x) a) z) v"

ex1 :: Term
ex1 = fromRight (Var "error") (parseTerm ex1_str)

-- Cyclic non-termination
omega_str :: String
omega_str = "(λx. x x)(λx. x x)"

omega :: Term
omega = fromRight (Var "error") (parseTerm omega_str)

-- Infinite expansion non-termination
selfApplication_str :: String
selfApplication_str = "(λx. (x x) x)(λx. (x x) x)"

selfApplication :: Term
selfApplication = fromRight (Var "error") (parseTerm selfApplication_str)

-- Cyclic if we are stubborn on the right redex (which is in fact the omega)
ex2_str :: String
ex2_str = "(λu. v)((λx. x x)(λx. x x))"

ex2 :: Term
ex2 = fromRight (Var "error") (parseTerm ex2_str)

churchNine_str :: String
churchNine_str = "(λm.λn.λf.m (n f)) (λf.λx.f (f (f x))) (λf.λx.f ( f (f x)))"
  where 
    mult = "(λn.λf.λx.f (n f x))"
    three = "(λf.λx.f(f(f x)))"

churchNine :: Term
churchNine = fromRight (Var "error") (parseTerm churchNine_str)



