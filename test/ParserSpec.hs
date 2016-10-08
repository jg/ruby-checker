module ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Parser

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses assignment" $ do
      parseString "variable = 12" `shouldBe` [(Assign "variable" (IntConst 12))]
    it "parses return" $ do
      parseString "return" `shouldBe` [(Return)]
    it "parses sequence of statements" $ do
      parseString "return; return" `shouldBe` [(Return), (Return)]
    it "parses if-then-else" $ do
      parseString "if true then return else return" `shouldBe` [If (BoolConst True) (Return) (Return)]
    it "parses if-then-else with parens" $ do
      parseString "if (true) then return else return" `shouldBe` [If (BoolConst True) (Return) (Return)]
    it "parses while" $ do
      parseString "while (true) do return" `shouldBe` [While (BoolConst True) Return]
    it "handles comments" $ do
      parseString "# (Return)\n return" `shouldBe` [Return]
    it "handles require" $ do
      parseString "require \"Stuff\"" `shouldBe` [Require "Stuff"]
    it "handles function definition" $ do
      let program = unlines ["def foo(a, b)",
                             "x = 1",
                             "end"]
      parseString program `shouldBe` [Method [(Var "a"), (Var "b")] [(Assign "x" (IntConst 1))]]
    it "handles function definition - empty arg list" $ do
      let program = unlines ["def foo()",
                             "x = 1",
                             "end"]
      parseString program `shouldBe` [Method [] [(Assign "x" (IntConst 1))]]
