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
    it "handles class definition - with superclass" $ do
      let program = unlines [
            "class Foo < Bar",
                "return",
                "def foo(a, b)",
                    "if a < 3 then",
                      "x = 3",
                    "else",
                      "x = a * 2",
                    "end",
                "end",
            "end"]
      parseString program `shouldBe` ClassDefinition "Foo" (Just "Bar") (Seq [Return (Info {sourceName = "", lineNumber = 2, columnNumber = 1}),Method [Var "a",Var "b"] (Seq [If (RBinary Less (Var "a") (IntConst 3) (Info {sourceName = "", lineNumber = 4, columnNumber = 4})) (Seq [Assign "x" (IntConst 3) (Info {sourceName = "", lineNumber = 5, columnNumber = 1})]) (Seq [Assign "x" (ABinary Multiply (Var "a") (IntConst 2) (Info {sourceName = "", lineNumber = 7, columnNumber = 7})) (Info {sourceName = "", lineNumber = 7, columnNumber = 1})]) (Info {sourceName = "", lineNumber = 4, columnNumber = 1})]) (Info {sourceName = "", lineNumber = 3, columnNumber = 1})]) (Info {sourceName = "", lineNumber = 1, columnNumber = 1})
  -- todo: empty class / function case
