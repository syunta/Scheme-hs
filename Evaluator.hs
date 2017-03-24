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

eval :: SObj -> Env -> SObj
-- self evaluating
eval (SSymbol x) env = (SSymbol x)
eval (SInt x)    env = (SInt x)
-- quotation
eval (SList [SSymbol "quote", exp] _) env = exp
-- application
eval (SList (op:args) Nil) env = apply (eval op env) (map (flip eval env) args)

apply :: SObj -> [SObj] -> SObj
apply (Primitive x f) args = f args
