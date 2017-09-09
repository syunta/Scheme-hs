module Subr
(
  primitiveProcedures, lookupPrimitive
) where

import Types
import qualified Data.Map as M

primitiveProcedures :: M.Map SObj ([SObj] -> SObj)
primitiveProcedures = M.fromList
  [(Primitive "+", primPlus),
   (Primitive "-", primMinus),
   (Primitive "*", primSub),
   (Primitive "/", primDiv),
   (Primitive "=", primE)]

lookupPrimitive :: SObj -> Maybe ([SObj] -> SObj)
lookupPrimitive key = M.lookup key primitiveProcedures

primPlus :: [SObj] -> SObj
primPlus = foldl1 (calc (+))

primMinus :: [SObj] -> SObj
primMinus = foldl1 (calc (-))

primSub :: [SObj] -> SObj
primSub = foldl1 (calc (*))

primDiv :: [SObj] -> SObj
primDiv = foldl1 (calc div)

calc :: (Int -> Int -> Int) -> (SObj -> SObj -> SObj)
calc f = fn
  where
    fn (SInt x) (SInt y) = SInt (f x y)

primE :: [SObj] -> SObj
primE [SInt x, SInt y] = SBool (x == y)
