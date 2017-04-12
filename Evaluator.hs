module Evaluator
(
  eval, initialEnv
) where

import Types
import Parser
import Subr

initialEnv :: Env
initialEnv = extendEnv primitiveProcedureNames primitiveProcedureObjects []

primitiveProcedureNames :: [String]
primitiveProcedureNames = map (\(n, _ , _) -> n) primitiveProcedures

primitiveProcedureObjects :: [SObj]
primitiveProcedureObjects = map (\(_, p , _) -> p) primitiveProcedures

extendEnv :: [String] -> [SObj] -> Env -> Env
extendEnv vars vals e = (vars, vals) : e

lookupEnv :: String -> Env -> SObj
lookupEnv _ [] = Nil -- TODO: Unbound variable error
lookupEnv x ((vars, vals):fs) = scanEnv vars vals
  where
    scanEnv [] _ = lookupEnv x fs
    scanEnv (var:vrs) (val:vls)
      | x == var = val
      | otherwise = scanEnv vrs vls

eval :: (SObj, Env) -> (SObj, Env)
-- self evaluating
eval (SInt x, env) = (SInt x, env)
eval (SBool x, env) = (SBool x, env)
-- variable
eval (SSymbol x, env) = (lookupEnv x env, env)
-- quotation
eval (SList [SSymbol "quote", exp] _, env) = (exp, env)
-- assignment
eval (SList ((SSymbol "set!"):exps) _, env) = evalSet (exps, env)
-- definition
eval (SList ((SSymbol "define"):exps) _, env) = evalDef (exps, env)
-- if
eval (SList ((SSymbol "if"):exps) _, env) = evalIf (exps, env)
-- lambda
eval (SList ((SSymbol "lambda"):exps) _, env) = (makeLambda exps env, env)
-- begin
eval (SList ((SSymbol "begin"):exps) _, env) = evalSeq (exps, env)
-- application
eval (SList (op:args) _, env) =
  let (op', env')    = eval (op, env)
      (args', env'') = evalArgs (args, env') in
  apply op' args' env''

evalArgs :: ([SObj], Env) -> ([SObj], Env)
evalArgs (xs, env) = iter ([], env) xs
  where
    iter (ys, env) []         = (reverse ys, env)
    iter (ys, env) (exp:exps) =
      let (exp', env') = eval (exp, env) in
      iter ((exp':ys), env') exps

apply :: SObj -> [SObj] -> Env -> (SObj, Env)
apply (Primitive x) args env = ((getProc x primitiveProcedures) args, env)
apply (SLambda ps "" body e1) args e2 = (fst . evalSeq $ (body, extendEnv ps args e1), e2)

evalIf :: ([SObj], Env) -> (SObj, Env)
evalIf ([pred, cnsq], env) =
  let result = eval (pred, env) in
    case result of
      (SBool False, env) -> ((SBool False), env)
      _                    -> eval (cnsq, env)
evalIf ([pred, cnsq, alt], env) =
  let result = eval (pred, env) in
    case result of
      (SBool False, env) -> eval (alt, env)
      _                  -> eval (cnsq, env)

makeLambda :: [SObj] -> Env -> SObj
makeLambda (Nil:body) = SLambda [] "" body
makeLambda ((SList exps Nil):body) = SLambda (makeParams exps) "" body
makeLambda ((SList exps (SSymbol tail)):body) = SLambda (makeParams exps) tail body

makeParams :: [SObj] -> [String]
makeParams [] = []
makeParams ((SSymbol x):xs) = x : makeParams xs

evalSeq :: ([SObj], Env) -> (SObj, Env)
evalSeq (xs, env) = iter (Nil, env) xs
  where
    iter x        []         = x
    iter (_, env) (exp:exps) = iter (eval (exp, env)) exps

evalSet :: ([SObj], Env) -> (SObj, Env)
evalSet ([SSymbol var, val], env) = (SSymbol "ok", setVar var val env)

setVar :: String -> SObj -> Env -> Env
setVar var val [] = [] -- TODO: Symbol not found error
setVar var val ((vars, vals):fs)
  | var `elem` vars = (replaceVal var val (vars, vals)) : fs
  | otherwise       = (vars, vals) : setVar var val fs

evalDef :: ([SObj], Env) -> (SObj, Env)
evalDef ([SSymbol var, val], env) = (SSymbol "ok", defineVar var val env)

defineVar :: String -> SObj -> Env -> Env
defineVar var val (([], _):fs) = ([var], [val]) : fs
defineVar var val (f:fs) = (replaceVal var val f) : fs

replaceVal :: String -> SObj -> Frame -> Frame
replaceVal var val (vars, vals) = makeFrame var val vars vals [] []

makeFrame :: String -> SObj -> [String] -> [SObj] -> [String] -> [SObj] -> Frame
makeFrame var val [] _ vars vals = (var:vars, val:vals)
makeFrame var val (x:xs) (y:ys) vars vals
  | x == var  = (var:xs ++ vars, val:ys ++ vals)
  | otherwise = makeFrame var val xs ys (x:vars) (y:vals)
