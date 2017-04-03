import Test.Hspec
import Parser
import Types

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "parses list to sobj" $ do
      parse "(1)" `shouldBe` (SList [SInt 1] Nil, [])
      parse "(1 2)" `shouldBe` (SList [SInt 1, SInt 2] Nil, [])
      parse "(1 (2 3))" `shouldBe` (SList [SInt 1, SList [SInt 2, SInt 3] Nil] Nil, [])
    it "parses empty list to sobj" $ do
      parse "()" `shouldBe` (SList [] Nil, [])
      parse "'()" `shouldBe` (SList [SSymbol "quote", SList [] Nil] Nil, [])
    it "parses quote to sobj" $ do
      parse "'hoge" `shouldBe` (SList [SSymbol "quote", SSymbol "hoge"] Nil, [])
      parse "'(hoge)" `shouldBe` (SList [SSymbol "quote", SList [SSymbol "hoge"] Nil] Nil, [])
      parse "('hoge)" `shouldBe` (SList [SList [SSymbol "quote", SSymbol "hoge"] Nil] Nil, [])
    it "parses dotted list to sobj" $ do
      parse "(1 . 2)" `shouldBe` (SList [SInt 1] (SInt 2), [])
    it "parses number to sobj" $ do
      parse "1" `shouldBe` (SInt 1, [])
    it "parses variable to sobj" $ do
      parse "a" `shouldBe` (SSymbol "a", [])
      parse "(+ x 2)" `shouldBe` (SList [SSymbol "+", SSymbol "x", SInt 2] Nil, [])
    it "parses bool to sobj" $ do
      parse "#t" `shouldBe` (SBool True, [])
      parse "#f" `shouldBe` (SBool False, [])
    it "parses a unit of syntax" $ do
      parse "1 2" `shouldBe` (SInt 1, ["2"])
      parse "(1) (2 3)" `shouldBe` (SList [SInt 1] Nil, ["(", "2", "3", ")"])
