import Parser
import Evaluator
import Types
import Env

run :: Env -> IO ()
run env = do
  x <- readPrompt
  case x of
    "exit" -> return ()
    ""     -> run env
    _      -> do
      let (expr, rest)  = parse x
          (val, newEnv) = evl (expr, env)
      putStrLn (show val)
      case rest of
        [] -> run newEnv
        _  -> do newEnv' <- evalRestPrint (rest, newEnv)
                 run newEnv'

readPrompt :: IO String
readPrompt = do
  putStr "scheme>"
  getLine

evalRestPrint :: ([String], Env) -> IO Env
evalRestPrint (x, env) = do
  let (expr, rest) = parseTokens x
      (val, newEnv)   = evl (expr, env)
  putStrLn (show val)
  case rest of
    [] -> return newEnv
    _  -> evalRestPrint (rest, newEnv)

main :: IO ()
main = run initialEnv
