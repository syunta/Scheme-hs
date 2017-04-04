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
primitiveProcedureNames = map fst primitiveProcedures

primitiveProcedureObjects :: [SObj]
primitiveProcedureObjects = map snd primitiveProcedures

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
-- if
eval (SList ((SSymbol "if"):exps) _, env) = evalIf (exps, env)
-- application
eval (SList (op:args) _, env) =
  (apply ((fst . eval) (op, env)) (map (fst . eval) (map ((flip (,)) env) args)), env)

apply :: SObj -> [SObj] -> SObj
apply (Primitive x f) args = f args

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
