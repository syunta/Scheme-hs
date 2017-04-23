module Env
(
  makeFrame
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
lookupEnv x (Node f e) [] = M.lookup x f
lookupEnv x e (r:rs) = do
  f <- scanEnv (Just e) (r:rs)
  let val = M.lookup x f
  case val of
    Nothing -> lookupEnv x e rs
    _       -> val

scanEnv :: Maybe Env -> Ref -> Maybe Frame
scanEnv (Just (Node f _)) []     = (Just f)
scanEnv (Just (Node _ e)) (r:rs) = scanEnv (M.lookup r e) rs
