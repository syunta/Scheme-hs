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
    it "evaluates quoted expression" $ do
      eval (parse' "'10") initialEnv `shouldBe` parse' "10"
      eval (parse' "'(1 2 3)") initialEnv `shouldBe` parse' "(1 2 3)"
      eval (parse' "'hoge") initialEnv `shouldBe` parse' "hoge"
    it "evaluates if syntax" $ do
      eval (parse' "(if #f 3)") initialEnv `shouldBe` parse' "#f"
      eval (parse' "(if #t 3)") initialEnv `shouldBe` parse' "3"
      eval (parse' "(if 10 10)") initialEnv `shouldBe` parse' "10"
    it "evaluates primitive application" $ do
      eval (parse' "(+ 2 4)") initialEnv `shouldBe` parse' "6"
      eval (parse' "(+ 2 4 10)") initialEnv `shouldBe` parse' "16"
      eval (parse' "(- 6 4)") initialEnv `shouldBe` parse' "2"
      eval (parse' "(- 10 3 2)") initialEnv `shouldBe` parse' "5"
      eval (parse' "(* 2 3 5)") initialEnv `shouldBe` parse' "30"
      eval (parse' "(/ 10 2)") initialEnv `shouldBe` parse' "5"
