import Types
import Parser

eval :: SObj -> Env -> SObj
eval (SList (op:operands) _) env = primApply op operands -- application
eval exp env = exp -- self evaluating

primApply :: SObj -> [SObj] -> SObj
primApply (Primitive op) operands = op operands

primPlus :: [SObj] -> SObj -- TODO: monadic
primPlus args = foldl (\(SInt x) (SInt acc) -> SInt (x + acc)) (SInt 0) args
