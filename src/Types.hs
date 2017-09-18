module Types
(
  SObj(..),
  Env(..),
  SEnv(..),
  Frame, Body, Ref, Params, DotParam
) where

import qualified Data.Map as M

data SObj = SInt Int |
            SSymbol String |
            SList [SObj] SObj |
            SBool Bool |
            SLambda Params DotParam Body [Ref] |
            Nil |
            Primitive String
            deriving (Eq,Ord)

data Env = Node Frame (M.Map Ref Env) deriving (Show, Eq)

type Frame = M.Map String SObj
type Ref = Int
type SEnv = (Env, [Ref])
type Params = [String]
type DotParam = String
type Body = [SObj]

instance Show SObj where
  show Nil = "nil"
  show (SSymbol x) = x
  show (SInt x) = show x
  show (SBool True) = "#t"
  show (SBool False) = "#f"
  show (SLambda ps p _ _) = "<closure " ++ show (ps ++ [p]) ++ ">"
  show (Primitive x) = "<subr " ++ x ++ " >"
  show (SList (SSymbol "quote" : xs) Nil) = showElements xs
  show (SList xs Nil) = "(" ++ showElements xs ++ ")"
  show (SList xs obj) = "(" ++ showElements xs ++ " . " ++ show obj ++ ")"

showElements :: [SObj] -> String
showElements = unwords . map show
