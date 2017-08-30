module Env
(
  initialEnv, extendEnv, lookupEnv, defineVar, setVar, extendRef
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
extendEnv vars vals (Node f e) = Node f $ M.insert (keyPos e) (Node (makeFrame vars vals) M.empty) e

lookupEnv :: String -> Env -> Ref -> Maybe SObj
lookupEnv var (Node f _) [] = M.lookup var f
lookupEnv var e (r:rs) = do
  f <- scanEnv e (r:rs)
  let val = M.lookup var f
  case val of
    Nothing -> lookupEnv var e rs
    _       -> val

scanEnv :: Env -> Ref -> Maybe Frame
scanEnv (Node f _) []     = Just f
scanEnv (Node _ e) (r:rs) = M.lookup r e >>= flip scanEnv rs

bottomEnv :: Env -> Ref -> Maybe (M.Map Int Env)
bottomEnv (Node _ e) []     = Just e
bottomEnv (Node _ e) (r:rs) = M.lookup r e >>= flip bottomEnv rs

defineVar :: String -> SObj -> Env -> Ref -> Env
defineVar var val e r = do
  let ret = scanEnv e r
  case ret of
    Nothing -> e
    Just f  -> let newf = M.insert var val f in
                 replace newf e r

setVar :: String -> SObj -> Env -> Ref -> Env
setVar var val (Node f e) [] =
  let bind = M.lookup var f in
  case bind of
    Nothing -> Node f e
    Just _  -> let newf = M.insert var val f in
                 Node newf e
setVar var val e r = do
  let ret = scanEnv e r
  case ret of
    Nothing -> e
    Just f ->
      let bind = M.lookup var f in
      case bind of
        Nothing -> setVar var val e (tail r)
        Just _  -> let newf = M.insert var val f in
                    replace newf e r

replace :: Frame -> Env -> Ref -> Env
replace f (Node _ e) [] = Node f e
replace f (Node f' e) (r:rs) =
  let ret = M.lookup r e in
  case ret of
    Nothing -> Node f' e
    Just e' ->
      let newe = replace f e' rs in
      Node f' (M.insert r newe e)

extendRef :: Env -> Ref -> Ref
extendRef e rs = do
  let m = bottomEnv e rs
  case m of
    Nothing -> rs
    Just m'  -> keyPos m' : rs

keyPos :: M.Map k a -> Int
keyPos = (1+) . length . M.keys
