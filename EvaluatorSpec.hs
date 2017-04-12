import Test.Hspec
import Evaluator
import Parser
import Types

parse' :: String -> SObj
parse' = fst . parse

eval' :: (SObj, Env) -> SObj
eval' = fst . eval

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "evaluates self-evaluating-value" $ do
      eval' (parse' "10", initialEnv) `shouldBe` parse' "10"
    it "evaluates quoted expression" $ do
      eval' (parse' "'10", initialEnv) `shouldBe` parse' "10"
      eval' (parse' "'(1 2 3)", initialEnv) `shouldBe` parse' "(1 2 3)"
      eval' (parse' "'hoge", initialEnv) `shouldBe` parse' "hoge"
    it "evaluates set! syntax" $ do
      eval' (parse' "(begin (define x 1) (set! x 2) x)", initialEnv) `shouldBe` parse' "2"
      -- TODO : more spec
    it "evaluates define syntax" $ do
      eval' (parse' "(begin (define hoge 20) hoge)", initialEnv) `shouldBe` parse' "20"
      eval' (parse' "(begin (define hoge 20) (define hoge 10) hoge)", initialEnv) `shouldBe` parse' "10"
    it "evaluates if syntax" $ do
      eval' (parse' "(if #f 3)", initialEnv) `shouldBe` parse' "#f"
      eval' (parse' "(if #t 3)", initialEnv) `shouldBe` parse' "3"
      eval' (parse' "(if 10 10)", initialEnv) `shouldBe` parse' "10"
      eval' (parse' "(if #f 1 0)", initialEnv) `shouldBe` parse' "0"
      eval' (parse' "(if #t 1 0)", initialEnv) `shouldBe` parse' "1"
    it "evaluates lambda syntax" $ do
      eval' (parse' "(lambda () 1 2)", initialEnv) `shouldBe` SLambda [] "" [SInt 1, SInt 2] initialEnv
      eval' (parse' "(lambda (x) 1)", initialEnv) `shouldBe` SLambda ["x"] "" [SInt 1] initialEnv
      eval' (parse' "(lambda (x . args) 1)", initialEnv) `shouldBe` SLambda ["x"] "args" [SInt 1] initialEnv
    it "evaluates begin syntax" $ do
      eval' (parse' "(begin 1 2 3 4 5)", initialEnv) `shouldBe` parse' "5"
    it "evaluates primitive procedure" $ do
      eval' (parse' "(+ 2 4)", initialEnv) `shouldBe` parse' "6"
      eval' (parse' "(+ 2 4 10)", initialEnv) `shouldBe` parse' "16"
      eval' (parse' "(- 6 4)", initialEnv) `shouldBe` parse' "2"
      eval' (parse' "(- 10 3 2)", initialEnv) `shouldBe` parse' "5"
      eval' (parse' "(* 2 3 5)", initialEnv) `shouldBe` parse' "30"
      eval' (parse' "(/ 10 2)", initialEnv) `shouldBe` parse' "5"
    it "evaluates compound procedure" $ do
      eval' (parse' "((lambda (x y) (+ x y)) 3 5)", initialEnv) `shouldBe` parse' "8"
