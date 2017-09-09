module Evaluator
(
  evl
) where

import Types
import Subr
import Env
import Control.Monad.State

evl :: (SObj, Env) -> (SObj, Env)
evl (exp, env) = runState (eval exp []) env

eval :: SObj -> Ref -> State Env SObj
-- self evaluating
eval (SInt x) r = return $ SInt x
eval (SBool x) r = return $ SBool x
-- variable
eval (SSymbol x) r = do
  env <- get
  let val = lookupEnv x env r
  case val of
    Nothing -> return Nil
    Just v  -> return v
-- quotation
eval (SList [SSymbol "quote", exp] _) r = return exp
-- assignment
eval (SList (SSymbol "set!" : exps) _) r = evalSet exps r
-- definition
eval (SList (SSymbol "define" : exps) _) r = evalDef exps r
---- if
eval (SList (SSymbol "if" : exps) _) r = evalIf exps r
-- cond
eval (SList (SSymbol "cond" : exps) _) r = eval (cond2if $ SList (SSymbol "cond" : exps) Nil) r
-- lambda
eval (SList (SSymbol "lambda" : exps) _) r = return $ makeLambda exps r
-- begin
eval (SList (SSymbol "begin" : exps) _) r = evalSeq exps r
-- application
eval (SList (op:args) _) r = do
  op' <- eval op r
  args' <- evalArgs args r
  apply op' args' r

evalArgs :: [SObj] -> Ref -> State Env [SObj]
evalArgs xs r = mapM (flip eval $ r) xs

apply :: SObj -> [SObj] -> Ref -> State Env SObj
apply (Primitive x) args r = do
  let result = lookupPrimitive (Primitive x)
  case result of
    Nothing -> return Nil
    Just p  -> return (p args)

--apply (SLambda ps "" body lr) args e r =
--  let (v, e') = evalSeq body ee er in
--    (v, e')
--    where ee = extendEnv ps args e
--          er = extendRef e lr
--apply (SLambda ps p body lr) args e r =
--  let (v, e') = evalSeq body ee er in
--    (v, e')
--    where ee = extendEnv (ps ++ [p]) (take (length ps) args ++ [SList (drop (length ps) args) Nil]) e
--          er = extendRef e lr

evalIf :: [SObj] -> Ref -> State Env SObj
evalIf [pred, cnsq] r = do
  result <- eval pred r
  case result of
    SBool False -> return $ SBool False
    _           -> eval cnsq r
evalIf [pred, cnsq, alt] r = do
  result <- eval pred r
  case result of
    SBool False -> eval alt r
    _           -> eval cnsq r

cond2if :: SObj -> SObj
cond2if (SList (SSymbol "cond" : clauses) _) = expandClause clauses

expandClause :: [SObj] -> SObj
expandClause [] = SBool False
expandClause (SList (SSymbol "else" : clause) _ : _) = seq2begin clause
expandClause (SList (pred:cnsq) _ : clauses) = SList [SSymbol "if", pred, seq2begin cnsq, expandClause clauses] Nil

seq2begin :: [SObj] -> SObj
seq2begin []   = Nil
seq2begin exps = SList (SSymbol "begin" : exps) Nil

makeLambda :: Body -> Ref -> SObj
makeLambda (Nil:body) = SLambda [] "" body
makeLambda (SList exps Nil : body) = SLambda (makeParams exps) "" body
makeLambda (SList exps (SSymbol tail) : body) = SLambda (makeParams exps) tail body

makeParams :: [SObj] -> [String]
makeParams [] = []
makeParams (SSymbol x : xs) = x : makeParams xs

evalSeq :: [SObj] -> Ref -> State Env SObj
evalSeq exps r = foldM (\x y -> eval y r) Nil exps

evalSet :: [SObj] -> Ref -> State Env SObj
evalSet [SSymbol var, val] r = do
  val' <- eval val r
  env <- get
  put $ setVar var val' env r
  return $ SSymbol "ok"

evalDef :: [SObj] -> Ref -> State Env SObj
evalDef [SSymbol var, SList (SSymbol "lambda" : body) _] r = do
  env <- get
  put $ defineVar var (makeLambda body r) env r
  return $ SSymbol "ok"
evalDef (SList (SSymbol var : params) Nil : body) r = do
  env <- get
  put $ defineVar var (makeLambda (SList params Nil : body) r) env r
  return $ SSymbol "ok"
evalDef (SList (SSymbol var : params) tail : body) r = do
  env <- get
  put $ defineVar var (makeLambda (SList params tail : body) r) env r
  return $ SSymbol "ok"
evalDef [SSymbol var, val] r = do
  val' <- eval val r
  env <- get
  put $ defineVar var val' env r
  return $ SSymbol "ok"
