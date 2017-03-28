module Subr
(
  primitiveProcedures
) where

import Types

primitiveProcedures :: [(String, SObj)]
primitiveProcedures = [("+", Primitive "+" primPlus),
                       ("-", Primitive "-" primMinus),
                       ("*", Primitive "*" primSub)]

primPlus :: [SObj] -> SObj
primPlus args = foldl1 (calc (+)) args

primMinus :: [SObj] -> SObj
primMinus args = foldl1 (calc (-)) args

primSub :: [SObj] -> SObj
primSub args = foldl1 (calc (*)) args

calc :: (Int -> Int -> Int) -> (SObj -> SObj -> SObj)
calc f = fn
  where
    fn (SInt x) (SInt y) = SInt (f x y)