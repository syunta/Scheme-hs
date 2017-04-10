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
-- begin
eval (SList ((SSymbol "begin"):exps) _, env) = evalSeq (exps, env)
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

evalSeq :: ([SObj], Env) -> (SObj, Env)
evalSeq (xs, env) = iter (Nil, env) xs
  where
    iter x        []       = x
    iter (_, env) (exp:exps) = iter (eval (exp, env)) exps

evalDef :: ([SObj], Env) -> (SObj, Env)
evalDef ([SSymbol var, val], env) = (SSymbol "ok", defineVar var val env)

defineVar :: String -> SObj -> Env -> Env
defineVar var val [] = [([var], [val])]
defineVar var val ((vars, vals):fs) = (makeFrame var val vars vals [] []) : fs

makeFrame :: String -> SObj -> [String] -> [SObj] -> [String] -> [SObj] -> Frame
makeFrame var val [] _ vars vals = (var:vars, val:vals)
makeFrame var val (x:xs) (y:ys) vars vals
  | x == var  = (var:xs ++ vars, val:ys ++ vals)
  | otherwise = makeFrame var val xs ys (x:vars) (y:vals)
