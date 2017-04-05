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
        _  -> do evalPrint' rest
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

evalPrint' :: [String] -> IO [String]
evalPrint' x = do
  let (expr, rest) = parseTokens x
      (val, env)   = eval (expr, initialEnv)
  putStrLn (show val)
  return rest

main :: IO ()
main = run
