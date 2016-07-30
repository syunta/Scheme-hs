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
  case tokens of ("(":ts) -> parseList (SList [], ts)
                 (t:ts) -> (parseAtom t, ts)

parseList :: (SObj, [String]) -> (SObj, [String])
parseList (exps, ")":tokens) = (exps, tokens)
parseList (exps, ".":tokens) = parseDotList (exps, tokens)
parseList (exps, "(":tokens) =
  let (sls, restTokens) = parseList (SList [], tokens)
      (SList ls, restRestTokens) = parseList (exps, restTokens) in
  (SList (sls : ls), restRestTokens)
parseList (exps, token:tokens) =
  let result = parseList (exps, tokens) in
    case result of
      (SList ls, restTokens) -> (SList (parseAtom token : ls), restTokens)
      (SDotList ls l, restTokens) -> (SDotList (parseAtom token : ls) l, restTokens)

parseDotList :: (SObj, [String]) -> (SObj, [String])
parseDotList (SList ls, "(":tokens) =
  let (sls, ")":restTokens) = parseList (SList [], tokens) in
  (SDotList ls sls, restTokens)
parseDotList (SList ls, t:")":ts) = (SDotList ls (parseAtom t), ts)

parseAtom :: String -> SObj
parseAtom token
  | not . (Nothing ==) $ mval = let (Just val) = mval in (SInt val)
  | otherwise = SSymbol token
    where mval = (readMaybe token :: Maybe Int)
