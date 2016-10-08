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
      parseString "variable := 12" `shouldBe` (Assign "variable" (IntConst 12))
    it "parses skip" $ do
      parseString "skip" `shouldBe` (Skip)
    it "parses sequence of statements" $ do
      parseString "skip; skip" `shouldBe` Seq [(Skip), (Skip)]
    it "handles parens" $ do
      parseString "(skip)" `shouldBe` (Skip)
    it "parses if-then-else" $ do
      parseString "if true then skip else skip" `shouldBe` If (BoolConst True) (Skip) (Skip)
    it "parses if-then-else with parens" $ do
      parseString "if (true) then skip else skip" `shouldBe` If (BoolConst True) (Skip) (Skip)
    it "parses while" $ do
      parseString "while (true) do skip" `shouldBe` While (BoolConst True) Skip
    it "handles comments" $ do
      parseString "# (Skip)\n skip" `shouldBe` Skip
      
