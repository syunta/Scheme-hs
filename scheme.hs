data SObj = SInt Int |
            Cell (SObj, SObj) |
            Nil

instance Show SObj where
  show (SInt x) = show x
  show (Cell (x, Nil)) = "(" ++ show x ++ ")"
  show (Cell (x, y)) = "(" ++ show x ++ " " ++ show y ++ ")"
  show Nil = ""
