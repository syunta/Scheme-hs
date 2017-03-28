module Subr
(
  primitiveProcedures
) where

import Types

primitiveProcedures :: [(String, SObj)]
primitiveProcedures = [("+", Primitive "+" primPlus),
                       ("-", Primitive "-" primMinus)]

primPlus :: [SObj] -> SObj
primPlus args = foldl1 plus args

primMinus :: [SObj] -> SObj
primMinus args = foldl1 minus args

plus :: SObj -> SObj -> SObj
plus (SInt x) (SInt y) = SInt (x + y)

minus :: SObj -> SObj -> SObj
minus (SInt x) (SInt y) = SInt (x - y)
