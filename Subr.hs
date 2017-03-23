module Subr
(
  primitiveProcedures
) where

import Types

primitiveProcedures :: [(String, [SObj] -> SObj)]
primitiveProcedures = [("+", primPlus)]

primPlus :: [SObj] -> SObj
primPlus args = foldl plus (SInt 0) args

plus :: SObj -> SObj -> SObj
plus (SInt x) (SInt y) = SInt (x + y)
