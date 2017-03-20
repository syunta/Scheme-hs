import Test.Hspec
import Parser
import Types

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "parses list to sobj" $ do
      parse "(1 2)" `shouldBe` (SList [(SInt 1), (SInt 2)] Nil, [])
