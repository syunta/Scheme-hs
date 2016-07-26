data SObj = SInt Int |
            Cell (SObj, SObj) |
            Nil

instance Show SObj where
  show (SInt x) = show x
  show (Cell (x, Nil)) = "(" ++ show x ++ ")"
  show (Cell (x, (Cell (y, z)))) = "(" ++ show x ++ " " ++ show (Cell (y, z)) ++ ")"
  show (Cell (x, atom)) = "(" ++ show x ++ " . " ++ show atom ++ ")"
  show Nil = ""
