module Env
(
  makeFrame
) where

import Types
import qualified Data.Map as M

makeFrame :: [String] -> [SObj] -> Frame
makeFrame vars vals = M.fromList $ zip vars vals
