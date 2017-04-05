import Parser
import Evaluator

run :: IO ()
run = do
  x <- readPrompt
  case x of
    "exit" -> return ()
    ""     -> run
    _      -> do
      let (expr, rest) = parse x
          (val, env)   = eval (expr, initialEnv)
      putStrLn (show val)
      case rest of
        [] -> run
        _  -> do evalRestPrint rest
                 run

readPrompt :: IO String
readPrompt = do
  putStr "scheme>"
  getLine

evalPrint :: String -> IO [String]
evalPrint x = do
  let (expr, rest) = parse x
      (val, env)   = eval (expr, initialEnv)
  putStrLn (show val)
  return rest

evalRestPrint :: [String] -> IO ()
evalRestPrint x = do
  let (expr, rest) = parseTokens x
      (val, env)   = eval (expr, initialEnv)
  putStrLn (show val)
  case rest of
    [] -> return ()
    _  -> evalRestPrint rest

main :: IO ()
main = run
