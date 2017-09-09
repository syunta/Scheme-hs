module EvaluatorSpec (spec) where

import Test.Hspec
import Evaluator
import Parser
import Types
import Env

parse' :: String -> SObj
parse' = fst . parse

eval' :: (SObj, Env) -> SObj
eval' = fst . evl

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates self-evaluating-value" $ do
      eval' (parse' "10", initialEnv) `shouldBe` parse' "10"
    it "evaluates quoted expression" $ do
      eval' (parse' "'10", initialEnv) `shouldBe` parse' "10"
      eval' (parse' "'(1 2 3)", initialEnv) `shouldBe` parse' "(1 2 3)"
      eval' (parse' "'hoge", initialEnv) `shouldBe` parse' "hoge"
--    it "evaluates set! syntax" $ do
--      eval' (parse' "(begin (define x 1) (set! x 2) x)", initialEnv) `shouldBe` parse' "2"
--      eval' (parse' "(begin (define x 1) (set! x (+ x 1)) x)", initialEnv) `shouldBe` parse' "2"
--      eval' (parse' "(begin (define c 0) (define (plus!) (set! c (+ c 1)) +) (define (one!) (set! c (+ c 1)) 1) ((plus!) (one!) (one!)) c)", initialEnv) `shouldBe` parse' "3"
--    it "evaluates define syntax" $ do
--      eval' (parse' "(begin (define hoge 20) hoge)", initialEnv) `shouldBe` parse' "20"
--      eval' (parse' "(begin (define hoge 20) (define hoge 10) hoge)", initialEnv) `shouldBe` parse' "10"
--      eval' (parse' "(begin (define hoge (+ 9 1)) hoge)", initialEnv) `shouldBe` parse' "10"
    it "evaluates if syntax" $ do
      eval' (parse' "(if #f 3)", initialEnv) `shouldBe` parse' "#f"
      eval' (parse' "(if #t 3)", initialEnv) `shouldBe` parse' "3"
      eval' (parse' "(if 10 10)", initialEnv) `shouldBe` parse' "10"
      eval' (parse' "(if #f 1 0)", initialEnv) `shouldBe` parse' "0"
      eval' (parse' "(if #t 1 0)", initialEnv) `shouldBe` parse' "1"
--    it "evaluates cond syntax" $ do
--      eval' (parse' "(cond ((= 0 1) 0) (else 1))", initialEnv) `shouldBe` parse' "1"
--      eval' (parse' "(cond ((= 0 1) 0))", initialEnv) `shouldBe` parse' "#f"
--      eval' (parse' "(cond ((= 0 1) 0) ((= 1 1) 0 (+ 2 1)) (else 9))", initialEnv) `shouldBe` parse' "3"
--    it "evaluates lambda syntax" $ do
--      eval' (parse' "(lambda () 1 2)", initialEnv) `shouldBe` SLambda [] "" [SInt 1, SInt 2] []
--      eval' (parse' "(lambda (x) 1)", initialEnv) `shouldBe` SLambda ["x"] "" [SInt 1] []
--      eval' (parse' "(lambda (x . args) 1)", initialEnv) `shouldBe` SLambda ["x"] "args" [SInt 1] []
    it "evaluates begin syntax" $ do
      eval' (parse' "(begin 1 2 3 4 5)", initialEnv) `shouldBe` parse' "5"
    it "evaluates primitive procedure" $ do
      eval' (parse' "(+ 2 4)", initialEnv) `shouldBe` parse' "6"
      eval' (parse' "(+ 2 4 10)", initialEnv) `shouldBe` parse' "16"
      eval' (parse' "(- 6 4)", initialEnv) `shouldBe` parse' "2"
      eval' (parse' "(- 10 3 2)", initialEnv) `shouldBe` parse' "5"
      eval' (parse' "(* 2 3 5)", initialEnv) `shouldBe` parse' "30"
      eval' (parse' "(/ 10 2)", initialEnv) `shouldBe` parse' "5"
--    it "evaluates compound procedure" $ do
--      eval' (parse' "((lambda (x y) (+ x y)) 3 5)", initialEnv) `shouldBe` parse' "8"
--      eval' (parse' "(begin (define plus (lambda (x y) (+ x y))) (plus 3 5))", initialEnv) `shouldBe` parse' "8"
--      eval' (parse' "(begin (define (plus x y) (+ x y)) (plus 2 5))", initialEnv) `shouldBe` parse' "7"
--      eval' (parse' "(begin (define (tail x . args) args) (tail 2 5 9))", initialEnv) `shouldBe` parse' "(5 9)"
--      eval'
--        (parse' "(begin (define (fib n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))", initialEnv)
--        `shouldBe` parse' "55"
