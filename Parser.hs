module Parser
(
  parse
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
      t ("", ys) = [[head ys]] ++ separate (tail ys)
      t (xs, ys) = [xs, [head ys]] ++ separate (tail ys)

parse :: String -> (SObj, [String])
parse str = let tokens = tokenize str in
  case tokens of ("(":ts) -> parseList (SList [] Nil, ts)
                 (t:ts) -> (parseAtom t, ts)

parseList :: (SObj, [String]) -> (SObj, [String])
parseList (exps, ")":tokens) = (exps, tokens)
parseList (exps, ".":tokens) = parseDotList (exps, tokens)
parseList (exps, "(":tokens) =
  let (sls, restTokens) = parseList (SList [] Nil, tokens)
      (SList ls obj, restRestTokens) = parseList (exps, restTokens) in
  (SList (sls : ls) obj, restRestTokens)
parseList (exps, token:tokens) =
  let result = parseList (exps, tokens) in
    case result of
      (SList ls obj, restTokens) -> (SList (parseAtom token : ls) obj, restTokens)

parseDotList :: (SObj, [String]) -> (SObj, [String])
parseDotList (SList ls _, "(":tokens) =
  let (sls, ")":restTokens) = parseList (SList [] Nil, tokens) in
  (SList ls sls, restTokens)
parseDotList (SList ls _, t:")":ts) = (SList ls (parseAtom t), ts)

parseAtom :: String -> SObj
parseAtom token
  | not . (Nothing ==) $ mval = let (Just val) = mval in (SInt val)
  | otherwise = SSymbol token
    where mval = (readMaybe token :: Maybe Int)
