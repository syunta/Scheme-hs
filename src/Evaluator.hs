module Evaluator
(
  evl
) where

import Types
import Subr
import Env

evl :: (SObj, Env) -> (SObj, Env)
evl (exp, env) = eval (exp, env, [])

eval :: (SObj, Env, Ref) -> (SObj, Env)
-- self evaluating
eval (SInt x, env, r) = (SInt x, env)
eval (SBool x, env, r) = (SBool x, env)
-- variable
eval (SSymbol x, env, r) = do
  let val = lookupEnv x env r
  case val of
    Nothing -> (Nil, env)
    Just v  -> (v, env)
-- quotation
eval (SList [SSymbol "quote", exp] _, env, r) = (exp, env)
-- assignment
eval (SList (SSymbol "set!" : exps) _, env, r) = evalSet (exps, env, r)
-- definition
eval (SList (SSymbol "define" : exps) _, env, r) = evalDef (exps, env, r)
-- if
eval (SList (SSymbol "if" : exps) _, env, r) = evalIf (exps, env, r)
-- cond
eval (SList (SSymbol "cond" : exps) _, env, r) = eval (cond2if $ SList (SSymbol "cond" : exps) Nil, env, r)
-- lambda
eval (SList (SSymbol "lambda" : exps) _, env, r) = (makeLambda exps r, env)
-- begin
eval (SList (SSymbol "begin" : exps) _, env, r) = evalSeq exps env r
-- application
eval (SList (op:args) _, env, r) =
  let (op', env')    = eval (op, env, r)
      (args', env'') = evalArgs (args, env', r) in
  apply op' args' env'' r

evalArgs :: ([SObj], Env, Ref) -> ([SObj], Env)
evalArgs (xs, env, r) = iter ([], env) xs
  where
    iter (ys, env) []         = (reverse ys, env)
    iter (ys, env) (exp:exps) =
      let (exp', env') = eval (exp, env, r) in
          iter (exp' : ys, env') exps

apply :: SObj -> [SObj] -> Env -> Ref -> (SObj, Env)
apply (Primitive x) args env r = (getProc x primitiveProcedures args, env)
apply (SLambda ps "" body lr) args e r =
  let (v, e') = evalSeq body ee er in
    (v, e')
    where ee = extendEnv ps args e
          er = extendRef e lr
apply (SLambda ps p body lr) args e r =
  let (v, e') = evalSeq body ee er in
    (v, e')
    where ee = extendEnv (ps ++ [p]) (take (length ps) args ++ [SList (drop (length ps) args) Nil]) e
          er = extendRef e lr

evalIf :: ([SObj], Env, Ref) -> (SObj, Env)
evalIf ([pred, cnsq], env, r) =
  let result = eval (pred, env, r) in
    case result of
      (SBool False, env) -> (SBool False, env)
      _                  -> eval (cnsq, env, r)
evalIf ([pred, cnsq, alt], env, r) =
  let result = eval (pred, env, r) in
    case result of
      (SBool False, env) -> eval (alt, env, r)
      _                  -> eval (cnsq, env, r)

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

evalSeq :: [SObj] -> Env -> Ref -> (SObj, Env)
evalSeq xs env r = iter (Nil, env) xs
  -- iter :: (SObj, Env) -> [SObj] -> (SObj, Env)
  where
    iter x        []         = x
    iter (_, env) (exp:exps) = iter (eval (exp, env, r)) exps

evalSet :: ([SObj], Env, Ref) -> (SObj, Env)
evalSet ([SSymbol var, val], env, r) =
  let (val', env') = eval (val, env, r) in
  (SSymbol "ok", setVar var val' env' r)

evalDef :: ([SObj], Env, Ref) -> (SObj, Env)
evalDef ([SSymbol var, SList (SSymbol "lambda" : body) _], env, r) =
  (SSymbol "ok", defineVar var (makeLambda body r) env r)
evalDef (SList (SSymbol var : params) Nil : body, env, r) =
  (SSymbol "ok", defineVar var (makeLambda (SList params Nil : body) r) env r)
evalDef (SList (SSymbol var : params) tail : body, env, r) =
  (SSymbol "ok", defineVar var (makeLambda (SList params tail : body) r) env r)
evalDef ([SSymbol var, val], env, r) =
  let (val', env') = eval (val, env, r) in
  (SSymbol "ok", defineVar var val' env' r)
