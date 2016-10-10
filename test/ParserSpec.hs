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
  describe "statements" $ do
    it "parses assignment" $ do
      parseString "variable = 12" `shouldBe` Seq [(Assign "variable" (IntConst 12))]
    it "parses return" $ do
      parseString "return" `shouldBe` Seq [(Return)]
    it "parses if-then-else" $ do
      parseString "if true then return else return" `shouldBe` Seq [If (BoolConst True) (Return) (Return)]
    it "parses if-then-else with parens" $ do
      parseString "if (true) then return else return" `shouldBe` Seq [If (BoolConst True) (Return) (Return)]
    it "parses while" $ do
      parseString "while (true) do return" `shouldBe` Seq [While (BoolConst True) Return]
    it "handles comments" $ do
      parseString "# (Return)\n return" `shouldBe` Seq [Return]
    it "handles require" $ do
      parseString "require \"Stuff\"" `shouldBe` Seq [Require "Stuff"]
    it "handles function definition" $ do
      let program = unlines ["def foo(a, b)",
                             "x = 1",
                             "end"]
      parseString program `shouldBe` Seq [Method [(Var "a"), (Var "b")] (Seq [(Assign "x" (IntConst 1))])]
    it "handles function definition - empty arg list" $ do
      let program = unlines ["def foo()",
                             "x = 1",
                             "end"]
      parseString program `shouldBe` Seq [Method [] (Seq [(Assign "x" (IntConst 1))])]

  describe "class" $ do
    it "handles class definition" $ do
      let program = unlines [
            "class Foo",
                "def foo",
                    "x = 3",
                "end",
            "end"]
      parseString program `shouldBe` ClassDefinition "Foo" Nothing (Seq [Method [] (Seq [Assign "x" (IntConst 3)])])
    it "handles class definition - with superclass" $ do
      let program = unlines [
            "class Foo < Bar",
                "return",
                "def foo(a, b)",
                    "x = 3",
                "end",
            "end"]
      parseString program `shouldBe` ClassDefinition "Foo" (Just "Bar") (Seq [Return,Method [Var "a",Var "b"] (Seq [Assign "x" (IntConst 3)])])

  -- todo: empty class / function case
