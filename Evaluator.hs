import Types
import Parser

eval :: SObj -> Env -> SObj
eval exp env = exp -- self evaluating
