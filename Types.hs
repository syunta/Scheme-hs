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
            SBool Bool |
            Nil |
            Primitive String ([SObj] -> SObj)

type Env = [Frame]
type Frame = ([String], [SObj])

instance Show SObj where
  show (SSymbol x) = x
  show (SInt x) = show x
  show (SBool True) = "#t"
  show (SBool False) = "#f"
  show (Primitive x _) = show "<subr " ++ x ++ " >"
  show (SList ((SSymbol "quote"):xs) Nil) = showElements xs
  show (SList xs Nil) = "(" ++ (showElements xs) ++ ")"
  show (SList xs obj) = "(" ++ (showElements xs) ++ " . " ++ show obj ++ ")"

instance Eq SObj where -- to pass specs
  (SSymbol x) == (SSymbol y) = x == y
  (SSymbol x) == _           = False
  _           == (SSymbol y) = False
  (SInt x)    == (SInt y)    = x == y
  (SInt x)    == _           = False
  _           == (SInt y)    = False
  (SBool x)   == (SBool y)   = x == y
  (SBool x)   == _           = False
  _           == (SBool y)   = False
  Nil         == Nil         = True
  Nil         == _           = False
  _           == Nil         = False
  (Primitive x _) == (Primitive y _) = x == y
  (Primitive x _) == _               = False
  _               == (Primitive y _) = False
  (SList xs x)    == (SList ys y)    = x == y && xs == ys

showElements :: [SObj] -> String
showElements = (intercalate " ") . (map show)
