module Env
(
  makeFrame
) where

import Types
import qualified Data.Map as M

makeFrame :: [String] -> [SObj] -> Frame
makeFrame vars vals = M.fromList $ zip vars vals

extendEnv :: [String] -> [SObj] -> Env -> Env
extendEnv vars vals (Node f m) = Node f $ M.insert num (Node (makeFrame vars vals) M.empty) m
  where num = (1+) $ length . M.keys $ m
