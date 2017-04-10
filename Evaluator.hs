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
-- definition
eval (SList ((SSymbol "define"):exps) _, env) = evalDef (exps, env)
-- if
eval (SList ((SSymbol "if"):exps) _, env) = evalIf (exps, env)
-- application
eval (SList (op:args) _, env) =
  (apply ((fst . eval) (op, env)) (map (fst . eval) (map ((flip (,)) env) args)), env)

apply :: SObj -> [SObj] -> SObj
apply (Primitive x) args = (getProc x primitiveProcedures) args

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

evalDef :: ([SObj], Env) -> (SObj, Env)
evalDef ([var, val], env) = (SSymbol "ok", defineVar var val env)

defineVar :: SObj -> SObj -> Env -> Env
defineVar (SSymbol var) val [] = [([var], [val])]
defineVar (SSymbol var) val ((vars, vals):fs) = (makeFrame vars vals [] []) : fs
  where
    makeFrame [] _ xs ys = (var:xs, val:ys)
    makeFrame (x:vrs) (y:vls) xs ys
      | x == var  = (var:vrs ++ xs, val:vls ++ ys)
      | otherwise = makeFrame vrs vls (x:xs) (y:ys)
