module Types
(
  SObj(..)
) where

import Data.List

data SObj = SInt Int |
            SList [SObj] |
            SDotList [SObj] SObj

instance Show SObj where
  show (SInt x) = show x
  show (SList xs) = "(" ++ (showElements xs) ++ ")"
  show (SDotList xs y) = "(" ++ (showElements xs) ++ " . " ++ show y ++ ")"

showElements :: [SObj] -> String
showElements = (intercalate " ") . (map show)
