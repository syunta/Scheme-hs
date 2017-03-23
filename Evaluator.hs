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
eval (SList [SSymbol "quote", exp] _) env = exp
eval exp env = exp -- self evaluating