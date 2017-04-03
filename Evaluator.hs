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

eval :: SObj -> Env -> SObj
-- self evaluating
eval (SInt x) _ = (SInt x)
eval (SBool x) _ = (SBool x)
-- variable
eval (SSymbol x) env = lookupEnv x env
-- quotation
eval (SList [SSymbol "quote", exp] _) env = exp
-- if
eval (SList ((SSymbol "if"):exps) _) env = evalIf exps env
-- application
eval (SList (op:args) _) env = apply (eval op env) (map (flip eval env) args)

apply :: SObj -> [SObj] -> SObj
apply (Primitive x f) args = f args

evalIf :: [SObj] -> Env -> SObj
evalIf [pred, cnsq] env =
  let result = eval pred env in
    case result of
      (SBool False) -> (SBool False)
      _             -> eval cnsq env
evalIf [pred, cnsq, alt] env =
  let result = eval pred env in
    case result of
      (SBool False) -> eval alt env
      _             -> eval cnsq env
