import Types
import Parser
import Subr

eval :: SObj -> Env -> SObj
eval exp env = exp -- self evaluating