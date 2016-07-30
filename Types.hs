module Types
(
  SObj(..)
) where

import Data.List

data SObj = SInt Int |
            SList [SObj] |
            SDotList [SObj] SObj |
            SSymbol String

instance Show SObj where
  show (SSymbol x) = x
  show (SInt x) = show x
  show (SList xs) = "(" ++ (showElements xs) ++ ")"
  show (SDotList xs y) = "(" ++ (showElements xs) ++ " . " ++ show y ++ ")"

showElements :: [SObj] -> String
showElements = (intercalate " ") . (map show)
