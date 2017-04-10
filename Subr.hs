module Subr
(
  primitiveProcedures, getProc
) where

import Types

primitiveProcedures :: [(String, SObj, [SObj] -> SObj)]
primitiveProcedures = [("+", Primitive "+", primPlus),
                       ("-", Primitive "-", primMinus),
                       ("*", Primitive "*", primSub),
                       ("/", Primitive "/", primDiv)]

getProc :: String -> [(String, SObj, [SObj] -> SObj)] -> ([SObj] -> SObj)
getProc key ((x, _, f):xs)
  | key == x = f
  | otherwise = getProc key xs

primPlus :: [SObj] -> SObj
primPlus args = foldl1 (calc (+)) args

primMinus :: [SObj] -> SObj
primMinus args = foldl1 (calc (-)) args

primSub :: [SObj] -> SObj
primSub args = foldl1 (calc (*)) args

primDiv :: [SObj] -> SObj
primDiv args = foldl1 (calc div) args

calc :: (Int -> Int -> Int) -> (SObj -> SObj -> SObj)
calc f = fn
  where
    fn (SInt x) (SInt y) = SInt (f x y)