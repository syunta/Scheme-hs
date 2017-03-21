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
    it "parses dotted list to sobj" $ do
      parse "(1 . 2)" `shouldBe` (SList [SInt 1] (SInt 2), [])
    it "parses number to sobj" $ do
      parse "1" `shouldBe` (SInt 1, [])
    it "parses a unit of syntax" $ do
      parse "1 2" `shouldBe` (SInt 1, ["2"])
      parse "(1) (2 3)" `shouldBe` (SList [SInt 1] Nil, ["(", "2", "3", ")"])
