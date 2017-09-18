module Parser
(
  parse, parseTokens, tokenize
) where

import Types
import Text.Read

tokenize :: String -> [String]
tokenize = concatMap separate . words

separate :: String -> [String]
separate ts =
  t $ break (`elem` ".()'") ts
    where
      t ("", "") = []
      t (xs, "") = [xs]
      t ("", '(' : ')' : ys) = "()" : separate ys
      t ("", ys) = [head ys] : separate (tail ys)
      t (xs, ys) = [xs, [head ys]] ++ separate (tail ys)

parse :: String -> (SObj, [String])
parse str = parseTokens (tokenize str)

parseTokens :: [String] -> (SObj, [String])
parseTokens ("(":ts) = parseList (SList [] Nil, ts)
parseTokens ("'":ts) = parseQuote ts
parseTokens (t:ts)   = (parseAtom t, ts)

parseList :: (SObj, [String]) -> (SObj, [String])
parseList (SList exps tail, ")":ts) = (SList (reverse exps) tail, ts)
parseList (SList exps _,    ".":ts) =
  let (tail, restTokens) = parseTokens ts in
      parseList (SList exps tail, restTokens)
parseList (SList exps tail, ts) =
  let (exp, restTokens) = parseTokens ts in
      parseList (SList (exp : exps) tail, restTokens)

parseQuote :: [String] -> (SObj, [String])
parseQuote ts =
  let (exps, restTokens) = parseTokens ts in
    (SList [SSymbol "quote", exps] Nil, restTokens)

parseAtom :: String -> SObj
parseAtom token
  | (Nothing /=) mval = let (Just val) = mval in SInt val
  | token == "#t" = SBool True
  | token == "#f" = SBool False
  | token == "()" = Nil
  | otherwise = SSymbol token
    where mval = readMaybe token :: Maybe Int
