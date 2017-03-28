module Subr
(
  primitiveProcedures
) where

import Types

primitiveProcedures :: [(String, SObj)]
primitiveProcedures = [("+", Primitive "+" primPlus)]

primPlus :: [SObj] -> SObj
primPlus args = foldl1 plus args

plus :: SObj -> SObj -> SObj
plus (SInt x) (SInt y) = SInt (x + y)
