module Types
(
  SObj(..),
  Env,
  Frame
) where

import Data.List

data SObj = SInt Int |
            SSymbol String |
            SList [SObj] SObj |
            Nil

type Env = [Frame]
type Frame = ([String], [SObj])

instance Show SObj where
  show (SSymbol x) = x
  show (SInt x) = show x
  show (SList xs Nil) = "(" ++ (showElements xs) ++ ")"
  show (SList xs (SList [] Nil)) = "(" ++ (showElements xs) ++ ")"
  show (SList xs obj) = "(" ++ (showElements xs) ++ " . " ++ show obj ++ ")"

showElements :: [SObj] -> String
showElements = (intercalate " ") . (map show)
