module Subr
(
  primitiveProcedures, getProc
) where

import Types

primitiveProcedures :: [(String, SObj, [SObj] -> SObj)]
primitiveProcedures = [("+", Primitive "+", primPlus),
                       ("-", Primitive "-", primMinus),
                       ("*", Primitive "*", primSub),
                       ("/", Primitive "/", primDiv),
                       ("=", Primitive "=", primE)]

getProc :: String -> [(String, SObj, [SObj] -> SObj)] -> ([SObj] -> SObj)
getProc key ((x, _, f):xs)
  | key == x = f
  | otherwise = getProc key xs

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
