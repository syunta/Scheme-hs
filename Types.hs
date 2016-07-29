module Types
(
  SObj(..)
) where

import Data.List

data SObj = SInt Int |
            SList [SObj] |
            SDotList [SObj] SObj |
            Nil

instance Show SObj where
  show (SInt x) = show x
  show (SList xs) = "(" ++ ((intercalate " ") . (map show) $ xs) ++ ")"
  show (SDotList xs y) = "(" ++ ((intercalate " ") . (map show) $ xs) ++ " . " ++ show y ++ ")"
  show Nil = ""
