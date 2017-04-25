module Evaluator
(
  evl
) where

import Types
import Parser
import Subr
import Env

evl :: (SObj, Env) -> (SObj, Env)
evl (exp, env) = (\(v,e,_)-> (v,e)) . eval $ (exp, env, [])

eval :: (SObj, Env, Ref) -> (SObj, Env, Ref)
-- self evaluating
eval (SInt x, env, r) = (SInt x, env, r)
eval (SBool x, env, r) = (SBool x, env, r)
-- variable
eval (SSymbol x, env, r) = do
  let val = lookupEnv x env r
  case val of
    Nothing -> (Nil, env, r)
    Just v  -> (v, env, r)
-- quotation
eval (SList [SSymbol "quote", exp] _, env, r) = (exp, env, r)
-- assignment
eval (SList ((SSymbol "set!"):exps) _, env, r) = evalSet (exps, env, r)
-- definition
eval (SList ((SSymbol "define"):exps) _, env, r) = evalDef (exps, env, r)
-- if
eval (SList ((SSymbol "if"):exps) _, env, r) = evalIf (exps, env, r)
-- cond
eval (SList ((SSymbol "cond"):exps) _, env, r) = eval (cond2if $ SList ((SSymbol "cond"):exps) Nil, env, r)
-- lambda
eval (SList ((SSymbol "lambda"):exps) _, env, r) = (makeLambda exps r, env, r)
-- begin
eval (SList ((SSymbol "begin"):exps) _, env, r) = evalSeq (exps, env, r)
-- application
eval (SList (op:args) _, env, r) =
  let (op', env', _)    = eval (op, env, r)
      (args', env'', _) = evalArgs (args, env', r) in
  apply op' args' env'' r

evalArgs :: ([SObj], Env, Ref) -> ([SObj], Env, Ref)
evalArgs (xs, env, r) = iter ([], env) xs
  where
    iter (ys, env) []         = (reverse ys, env, r)
    iter (ys, env) (exp:exps) =
      let (exp', env', _) = eval (exp, env, r) in
      iter ((exp':ys), env') exps

apply :: SObj -> [SObj] -> Env -> Ref -> (SObj, Env, Ref)
apply (Primitive x) args env r = ((getProc x primitiveProcedures) args, env, r)
apply (SLambda ps "" body lr) args e r = ((\(x,_,_) -> x) . evalSeq $ (body, ee, er), e, r) -- TODO: Set!
  where ee = extendEnv ps args e
        er = extendRef e lr
apply (SLambda ps p body lr) args e r = ((\(x,_,_) -> x) . evalSeq $ (body, ee, er), e, r)
  where ee = extendEnv (ps ++ [p]) ((take (length ps) args) ++ [SList (drop (length ps) args) Nil]) e
        er = extendRef e lr

evalIf :: ([SObj], Env, Ref) -> (SObj, Env, Ref)
evalIf ([pred, cnsq], env, r) =
  let result = eval (pred, env, r) in
    case result of
      (SBool False, env, r) -> ((SBool False), env, r)
      _                     -> eval (cnsq, env, r)
evalIf ([pred, cnsq, alt], env, r) =
  let result = eval (pred, env, r) in
    case result of
      (SBool False, env, r) -> eval (alt, env, r)
      _                     -> eval (cnsq, env, r)

cond2if :: SObj -> SObj
cond2if (SList ((SSymbol "cond"):clauses) _) = expandClause clauses

expandClause :: [SObj] -> SObj
expandClause [] = SBool False
expandClause ((SList ((SSymbol "else"):clause) _):_) = seq2begin clause
expandClause ((SList (pred:cnsq) _):clauses) = SList [SSymbol "if", pred, seq2begin cnsq, expandClause clauses] Nil

seq2begin :: [SObj] -> SObj
seq2begin []   = Nil
seq2begin exps = SList ((SSymbol "begin"):exps) Nil

makeLambda :: Body -> Ref -> SObj
makeLambda (Nil:body) = SLambda [] "" body
makeLambda ((SList exps Nil):body) = SLambda (makeParams exps) "" body
makeLambda ((SList exps (SSymbol tail)):body) = SLambda (makeParams exps) tail body

makeParams :: [SObj] -> [String]
makeParams [] = []
makeParams ((SSymbol x):xs) = x : makeParams xs

evalSeq :: ([SObj], Env, Ref) -> (SObj, Env, Ref)
evalSeq (xs, env, r) = iter (Nil, env, r) xs
  where
    iter x        []         = x
    iter (_, env, r) (exp:exps) = iter (eval (exp, env, r)) exps

evalSet :: ([SObj], Env, Ref) -> (SObj, Env, Ref)
evalSet ([SSymbol var, val], env, r) =
  let (val', env', _) = eval (val, env, r) in
  (SSymbol "ok", setVar var val' env' r, r)

evalDef :: ([SObj], Env, Ref) -> (SObj, Env, Ref)
evalDef ([SSymbol var, SList ((SSymbol "lambda"):body) _], env, r) =
  (SSymbol "ok", defineVar var (makeLambda body r) env r, r)
evalDef (((SList ((SSymbol var):params) Nil):body), env, r) =
  (SSymbol "ok", defineVar var (makeLambda ((SList params Nil):body) r) env r, r)
evalDef (((SList ((SSymbol var):params) tail):body), env, r) =
  (SSymbol "ok", defineVar var (makeLambda ((SList params tail):body) r) env r, r)
evalDef ([SSymbol var, val], env, r) =
  let (val', env', _) = eval (val, env, r) in
  (SSymbol "ok", defineVar var val' env' r, r)
