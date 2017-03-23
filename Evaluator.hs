import Types
import Parser
import Subr

extendEnv :: [String] -> [SObj] -> Env -> Env
extendEnv vars vals e = (vars, vals) : e

eval :: SObj -> Env -> SObj
eval exp env = exp -- self evaluating