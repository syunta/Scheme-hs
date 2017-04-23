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
-- eval (SList ((SSymbol "set!"):exps) _, env) = evalSet (exps, env)
-- definition
-- eval (SList ((SSymbol "define"):exps) _, env) = evalDef (exps, env)
-- if
eval (SList ((SSymbol "if"):exps) _, env, r) = evalIf (exps, env, r)
-- cond
eval (SList ((SSymbol "cond"):exps) _, env, r) = eval (cond2if $ SList ((SSymbol "cond"):exps) Nil, env, r)
-- lambda
--eval (SList ((SSymbol "lambda"):exps) _, env) = (makeLambda exps env, env)
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
--apply (SLambda ps "" body _) args e2 = (fst . evalSeq $ (body, extendEnv ps args e2), e2) -- TODO: Replace Env
--apply (SLambda ps p body e1) args e2 =
--  (fst . evalSeq $ (body, extendEnv (ps ++ [p]) ((take (length ps) args) ++ [SList (drop (length ps) args) Nil]) e1), e2)

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

--makeLambda :: [SObj] -> Env -> SObj
--makeLambda (Nil:body) = SLambda [] "" body
--makeLambda ((SList exps Nil):body) = SLambda (makeParams exps) "" body
--makeLambda ((SList exps (SSymbol tail)):body) = SLambda (makeParams exps) tail body

--makeParams :: [SObj] -> [String]
--makeParams [] = []
--makeParams ((SSymbol x):xs) = x : makeParams xs

evalSeq :: ([SObj], Env, Ref) -> (SObj, Env, Ref)
evalSeq (xs, env, r) = iter (Nil, env, r) xs
  where
    iter x        []         = x
    iter (_, env, r) (exp:exps) = iter (eval (exp, env, r)) exps

--evalSet :: ([SObj], Env) -> (SObj, Env)
--evalSet ([SSymbol var, val], env) =
--  let (val', env') = eval (val, env) in
--  (SSymbol "ok", setVar var val' env')

--setVar :: String -> SObj -> Env -> Env
--setVar var val [] = [] -- TODO: Symbol not found error
--setVar var val ((vars, vals):fs)
--  | var `elem` vars = (replaceVal var val (vars, vals)) : fs
--  | otherwise       = (vars, vals) : setVar var val fs

--evalDef :: ([SObj], Env) -> (SObj, Env)
--evalDef ([SSymbol var, SList ((SSymbol "lambda"):body) _], env) =
--  (SSymbol "ok", defineVar var (makeLambda body env) env)
--evalDef (((SList ((SSymbol var):params) Nil):body), env) =
--  (SSymbol "ok", defineVar var (makeLambda ((SList params Nil):body) env) env)
--evalDef (((SList ((SSymbol var):params) tail):body), env) =
--  (SSymbol "ok", defineVar var (makeLambda ((SList params tail):body) env) env)
--evalDef ([SSymbol var, val], env) =
--  let (val', env') = eval (val, env) in
--  (SSymbol "ok", defineVar var val' env')

--defineVar :: String -> SObj -> Env -> Env
--defineVar var val (([], _):fs) = ([var], [val]) : fs
--defineVar var val (f:fs) = (replaceVal var val f) : fs

--replaceVal :: String -> SObj -> Frame -> Frame
--replaceVal var val (vars, vals) = makeFrame var val vars vals [] []

--makeFrame :: String -> SObj -> [String] -> [SObj] -> [String] -> [SObj] -> Frame
--makeFrame var val [] _ vars vals = (var:vars, val:vals)
--makeFrame var val (x:xs) (y:ys) vars vals
--  | x == var  = (var:xs ++ vars, val:ys ++ vals)
--  | otherwise = makeFrame var val xs ys (x:vars) (y:vals)
