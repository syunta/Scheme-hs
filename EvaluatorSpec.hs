import Test.Hspec
import Evaluator
import Parser
import Types

parse' :: String -> SObj
parse' = fst . parse

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "evaluates self-evaluating-value" $ do
      eval (parse' "10") initialEnv `shouldBe` parse' "10"
