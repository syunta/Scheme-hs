import Types
import Text.Read

tokenize :: String -> [String]
tokenize = concat . (map separate) . words

separate :: String -> [String]
separate ts =
  t $ span (not . (`elem` "()'")) ts
    where
      t ("", "") = []
      t (xs, "") = [xs]
      t ("", ys) = [[head ys]] ++ separate (tail ys)
      t (xs, ys) = [xs, [head ys]] ++ separate (tail ys)

parseList :: (SObj, [String]) -> (SObj, [String])
parseList (exps, ")":tokens) = (exps, tokens)
parseList (exps, "(":tokens) =
  let (sls, restTokens) = parseList (SList [], tokens)
      (SList ls, restRestTokens) = parseList (exps, restTokens) in
  (SList (sls : ls), restRestTokens)
parseList (exps, token:tokens) =
  let (SList ls, restTokens) = parseList (exps, tokens) in
  (SList (parseAtom token : ls), restTokens)

parseAtom :: String -> SObj
parseAtom token
  | not . (Nothing ==) $ mval = let (Just val) = mval in (SInt val)
    where mval = (readMaybe token :: Maybe Int)
