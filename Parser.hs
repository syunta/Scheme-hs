module Parser
(
  parse, parseTokens
) where

import Types
import Text.Read

tokenize :: String -> [String]
tokenize = concat . (map separate) . words

separate :: String -> [String]
separate ts =
  t $ span (not . (`elem` ".()'")) ts
    where
      t ("", "") = []
      t (xs, "") = [xs]
      t ("", ('(':')':ys)) = ["()"] ++ separate ys
      t ("", ys) = [[head ys]] ++ separate (tail ys)
      t (xs, ys) = [xs, [head ys]] ++ separate (tail ys)

parse :: String -> (SObj, [String])
parse str = parseTokens (tokenize str)

parseTokens :: [String] -> (SObj, [String])
parseTokens ("(":ts) = parseList (SList [] Nil, ts)
parseTokens ("'":ts) = parseQuote ts
parseTokens (t:ts)   = (parseAtom t, ts)

parseList :: (SObj, [String]) -> (SObj, [String])
parseList (exps, ")":ts) = (exps, ts)
parseList (exps, "'":ts) =
  let (qls, restTokens) = parseQuote ts
      (SList ls tail, restRestTokens) = parseList (exps, restTokens) in
  (SList (qls : ls) tail, restRestTokens)
parseList (exps, ".":ts) = parseDotList (exps, ts)
parseList (exps, "(":ts) =
  let (sls, restTokens) = parseList (SList [] Nil, ts)
      (SList ls tail, restRestTokens) = parseList (exps, restTokens) in
  (SList (sls : ls) tail, restRestTokens)
parseList (exps, t:ts) =
  let (SList ls tail, restTokens) = parseList (exps, ts) in
  (SList (parseAtom t : ls) tail, restTokens)

parseQuote :: [String] -> (SObj, [String])
parseQuote ts =
  let (exps, restTokens) = parseTokens ts in
    (SList [SSymbol "quote", exps] Nil, restTokens)

parseDotList :: (SObj, [String]) -> (SObj, [String])
parseDotList (SList ls1 _, "(":ts) =
  let (SList ls2 tail, ")":restTokens) = parseList (SList [] Nil, ts) in
  (SList (ls1 ++ ls2) tail, restTokens)
parseDotList (SList ls _, t:")":ts) = (SList ls (parseAtom t), ts)

parseAtom :: String -> SObj
parseAtom token
  | not . (Nothing ==) $ mval = let (Just val) = mval in (SInt val)
  | token == "#t" = SBool True
  | token == "#f" = SBool False
  | token == "()" = Nil
  | otherwise = SSymbol token
    where mval = (readMaybe token :: Maybe Int)
