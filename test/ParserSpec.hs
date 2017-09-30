module ParserSpec (spec) where

import Test.Hspec
import Parser
import Types

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses list to sobj" $ do
      parseExpr "(1)" `shouldBe` (SList [SInt 1] Nil, [])
      parseExpr "(1 2)" `shouldBe` (SList [SInt 1, SInt 2] Nil, [])
      parseExpr "(1 (2 3))" `shouldBe` (SList [SInt 1, SList [SInt 2, SInt 3] Nil] Nil, [])
    it "parseExprs empty list to sobj" $ do
      parseExpr "()" `shouldBe` (Nil, [])
      parseExpr "'()" `shouldBe` (SList [SSymbol "quote", Nil] Nil, [])
      parseExpr "(1 ())" `shouldBe` (SList [SInt 1, Nil] Nil, [])
    it "parseExprs quote to sobj" $ do
      parseExpr "'hoge" `shouldBe` (SList [SSymbol "quote", SSymbol "hoge"] Nil, [])
      parseExpr "'(hoge)" `shouldBe` (SList [SSymbol "quote", SList [SSymbol "hoge"] Nil] Nil, [])
      parseExpr "('hoge)" `shouldBe` (SList [SList [SSymbol "quote", SSymbol "hoge"] Nil] Nil, [])
    it "parseExprs dotted list to sobj" $ do
      parseExpr "(1 . 2)" `shouldBe` (SList [SInt 1] (SInt 2), [])
    it "parseExprs number to sobj" $ do
      parseExpr "1" `shouldBe` (SInt 1, [])
    it "parseExprs variable to sobj" $ do
      parseExpr "a" `shouldBe` (SSymbol "a", [])
      parseExpr "(+ x 2)" `shouldBe` (SList [SSymbol "+", SSymbol "x", SInt 2] Nil, [])
    it "parseExprs bool to sobj" $ do
      parseExpr "#t" `shouldBe` (SBool True, [])
      parseExpr "#f" `shouldBe` (SBool False, [])
    it "parseExprs a unit of syntax" $ do
      parseExpr "1 2" `shouldBe` (SInt 1, ["2"])
      parseExpr "(1) (2 3)" `shouldBe` (SList [SInt 1] Nil, ["(", "2", "3", ")"])
