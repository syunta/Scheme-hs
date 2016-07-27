import Types

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
