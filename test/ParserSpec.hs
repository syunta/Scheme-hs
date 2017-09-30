module ParserSpec (spec) where

import Test.Hspec
import Parser
import Types

parse :: String -> SObj
parse input = case parseExprs input of
                Right (expr, _) -> expr

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses list to sobj" $ do
      parse "(1)" `shouldBe` SList [SInt 1] Nil
      parse "(1 2)" `shouldBe` SList [SInt 1, SInt 2] Nil
      parse "(1 (2 3))" `shouldBe` SList [SInt 1, SList [SInt 2, SInt 3] Nil] Nil
    it "parse empty list to sobj" $ do
      parse "()" `shouldBe` Nil
      parse "'()" `shouldBe` SList [SSymbol "quote", Nil] Nil
      parse "(1 ())" `shouldBe` SList [SInt 1, Nil] Nil
    it "parse quote to sobj" $ do
      parse "'hoge" `shouldBe` SList [SSymbol "quote", SSymbol "hoge"] Nil
      parse "'(hoge)" `shouldBe` SList [SSymbol "quote", SList [SSymbol "hoge"] Nil] Nil
      parse "('hoge)" `shouldBe` SList [SList [SSymbol "quote", SSymbol "hoge"] Nil] Nil
    it "parse dotted list to sobj" $ do
      parse "(1 . 2)" `shouldBe` SList [SInt 1] (SInt 2)
    it "parse number to sobj" $ do
      parse "1" `shouldBe` SInt 1
    it "parse variable to sobj" $ do
      parse "a" `shouldBe` SSymbol "a"
      parse "(+ x 2)" `shouldBe` SList [SSymbol "+", SSymbol "x", SInt 2] Nil
    it "parse bool to sobj" $ do
      parse "#t" `shouldBe` SBool True
      parse "#f" `shouldBe` SBool False
    it "parse a unit of syntax" $ do
      parseExprs "1 2" `shouldBe` Right (SInt 1, " 2")
      parseExprs "(1) (2 3)" `shouldBe` Right (SList [SInt 1] Nil, " (2 3)")
