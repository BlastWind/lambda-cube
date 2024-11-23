module Untyped.Spec (module Untyped.Spec) where

import Data.Either (isRight)
import Test.Hspec
import Untyped 

untypedSpec :: Spec
untypedSpec =
  describe "Untyped Tests" $ do
    it "No Parsing Error" $ do
      all (isRight . parseTerm) [var1_str, var2_str, ex1_str, ex2_str, omega_str, selfApplication_str, churchNine_str]
    it "Parse Correctness" $ do
      var1 `shouldBe` Var "x"
      var2 `shouldBe` Lam "x" (Var "y")
      ex1 `shouldBe` App (Lam "x" (App (Lam "y" (App (App (Var "y") (Var "x")) (Var "a"))) (Var "z"))) (Var "v")
      ex2 `shouldBe` App (Lam "u" (Var "v")) (App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x"))))
      omega `shouldBe` App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x")))
      selfApplication `shouldBe` App (Lam "x" (App (App (Var "x") (Var "x")) (Var "x"))) (Lam "x" (App (App (Var "x") (Var "x")) (Var "x")))
      -- churchGen `shouldBe` App (Lam "n" (Lam "f" (Lam "x" (App (App (App (Var "f") (Var "n")) (Var "f")) (Var "x"))))) (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))))
    it "Reduction" $ do 
      reductionTest var1_str var1_str
      reductionTest var2_str var2_str
      reductionTest omega_str omega_str
      reductionTest ex1_str "z v a"
      reductionTest ex2_str "v"
      reductionTest churchNine_str "(λf.λx.f(f(f(f(f(f(f(f(f x)))))))))"


reductionTest :: String -> String -> Expectation
reductionTest from expected = 
    case parseTerm from of
      Right fromTerm -> case parseTerm expected of
        Right expectedTerm -> reduce fromTerm `shouldBe` expectedTerm
        Left _ -> error "Parsing failed: expected term"
      Left _ -> error "Parsing failed: from term" 