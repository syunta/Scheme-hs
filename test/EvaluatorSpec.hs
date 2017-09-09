module EvaluatorSpec (spec) where

import Test.Hspec
import Evaluator
import Parser
import Types
import Env

parse' :: String -> SObj
parse' = fst . parse

parseEval :: String -> SObj
parseEval exp = fst $ evl (parse' exp) initialEnv

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates self-evaluating-value" $ do
      parseEval "10" `shouldBe` parse' "10"
    it "evaluates quoted expression" $ do
      parseEval "'10" `shouldBe` parse' "10"
      parseEval "'(1 2 3)" `shouldBe` parse' "(1 2 3)"
      parseEval "'hoge" `shouldBe` parse' "hoge"
    it "evaluates set! syntax" $ do
      parseEval "(begin (define x 1) (set! x 2) x)" `shouldBe` parse' "2"
      parseEval "(begin (define x 1) (set! x (+ x 1)) x)" `shouldBe` parse' "2"
      parseEval "(begin (define c 0) (define (plus!) (set! c (+ c 1)) +) (define (one!) (set! c (+ c 1)) 1) ((plus!) (one!) (one!)) c)" `shouldBe` parse' "3"
    it "evaluates define syntax" $ do
      parseEval "(begin (define hoge 20) hoge)" `shouldBe` parse' "20"
      parseEval "(begin (define hoge 20) (define hoge 10) hoge)" `shouldBe` parse' "10"
      parseEval "(begin (define hoge (+ 9 1)) hoge)" `shouldBe` parse' "10"
    it "evaluates if syntax" $ do
      parseEval "(if #f 3)" `shouldBe` parse' "#f"
      parseEval "(if #t 3)" `shouldBe` parse' "3"
      parseEval "(if 10 10)" `shouldBe` parse' "10"
      parseEval "(if #f 1 0)" `shouldBe` parse' "0"
      parseEval "(if #t 1 0)" `shouldBe` parse' "1"
    it "evaluates cond syntax" $ do
      parseEval "(cond (#f 0) (else 1))" `shouldBe` parse' "1"
      parseEval "(cond ((= 0 1) 0) (else 1))" `shouldBe` parse' "1"
      parseEval "(cond ((= 0 1) 0))" `shouldBe` parse' "#f"
      parseEval "(cond ((= 0 1) 0) ((= 1 1) 0 (+ 2 1)) (else 9))" `shouldBe` parse' "3"
    it "evaluates lambda syntax" $ do
      parseEval "(lambda () 1 2)" `shouldBe` SLambda [] "" [SInt 1, SInt 2] []
      parseEval "(lambda (x) 1)" `shouldBe` SLambda ["x"] "" [SInt 1] []
      parseEval "(lambda (x . args) 1)" `shouldBe` SLambda ["x"] "args" [SInt 1] []
    it "evaluates begin syntax" $ do
      parseEval "(begin 1 2 3 4 5)" `shouldBe` parse' "5"
    it "evaluates primitive procedure" $ do
      parseEval "(+ 2 4)" `shouldBe` parse' "6"
      parseEval "(+ 2 4 10)" `shouldBe` parse' "16"
      parseEval "(- 6 4)" `shouldBe` parse' "2"
      parseEval "(- 10 3 2)" `shouldBe` parse' "5"
      parseEval "(* 2 3 5)" `shouldBe` parse' "30"
      parseEval "(/ 10 2)" `shouldBe` parse' "5"
    it "evaluates compound procedure" $ do
      parseEval "((lambda (x y) (+ x y)) 3 5)" `shouldBe` parse' "8"
      parseEval "(begin (define plus (lambda (x y) (+ x y))) (plus 3 5))" `shouldBe` parse' "8"
      parseEval "(begin (define (plus x y) (+ x y)) (plus 2 5))" `shouldBe` parse' "7"
      parseEval "(begin (define (tail x . args) args) (tail 2 5 9))" `shouldBe` parse' "(5 9)"
      parseEval "(begin (define (fib n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))" `shouldBe` parse' "55"
