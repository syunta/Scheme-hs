module Env
(
  initialEnv, extendEnv, lookupEnv, defineVar
) where

import Types
import Subr
import qualified Data.Map as M

initialEnv :: Env
initialEnv = Node (makeFrame primitiveProcedureNames primitiveProcedureObjects) M.empty

primitiveProcedureNames :: [String]
primitiveProcedureNames = map (\(n, _ , _) -> n) primitiveProcedures

primitiveProcedureObjects :: [SObj]
primitiveProcedureObjects = map (\(_, p , _) -> p) primitiveProcedures

makeFrame :: [String] -> [SObj] -> Frame
makeFrame vars vals = M.fromList $ zip vars vals

extendEnv :: [String] -> [SObj] -> Env -> Env
extendEnv vars vals (Node f e) = Node f $ M.insert num (Node (makeFrame vars vals) M.empty) e
  where num = (1+) $ length . M.keys $ e

lookupEnv :: String -> Env -> Ref -> Maybe SObj
lookupEnv var (Node f e) [] = M.lookup var f
lookupEnv var e (r:rs) = do
  f <- scanEnv (Just e) (r:rs)
  let val = M.lookup var f
  case val of
    Nothing -> lookupEnv var e rs
    _       -> val

scanEnv :: Maybe Env -> Ref -> Maybe Frame
scanEnv (Just (Node f _)) []     = (Just f)
scanEnv (Just (Node _ e)) (r:rs) = scanEnv (M.lookup r e) rs

defineVar :: String -> SObj -> Env -> Ref -> Env
defineVar var val e r = do
  let f = scanEnv (Just e) r
  case f of
    Nothing -> e
    (Just f) -> let newf = M.insert var val f in
                replace newf e r

replace :: Frame -> Env -> Ref -> Env
replace f (Node _ e) [] = Node f e
replace f (Node f' e) (r:rs) =
  let e' = M.lookup r e in
  case e' of
    Nothing -> Node f' e
    (Just e') ->
      let newe = replace f e' rs in
      Node f' (M.insert r newe e)
