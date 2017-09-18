import Parser
import Evaluator
import Types
import Env

run :: Env -> IO ()
run env = do
  input <- readPrompt
  case input of
    "exit" -> return ()
    ""     -> run env
    _      -> do
      let tokens = tokenize input
      newEnv <- evalPrint tokens env
      run newEnv

evalPrint :: [String] -> Env -> IO Env
evalPrint x env = do
  let (expr, rest) = parseTokens x
      result = evl expr env
  case result of
    Right (val, newEnv) -> do
      print val
      case rest of
        [] -> return newEnv
        _  -> evalPrint rest newEnv
    Left err -> do
      print err
      return env

readPrompt :: IO String
readPrompt = do
  putStr "scheme>"
  readLine

readLine :: IO String
readLine = readLine' ""

readLine' :: String -> IO String
readLine' xs = do
  x <- getChar
  case x of
    '\n'   -> return (reverse xs)
    '\DEL' -> do
       xs' <- deleteHead xs
       readLine' xs'
    _      -> readLine' (x:xs)

deleteHead :: String -> IO String
deleteHead [] = do
  putStr "\ESC[1D\ESC[1D  \ESC[1D\ESC[1D"
  return []
deleteHead (x:xs) = do
  putStr "\ESC[1D\ESC[1D\ESC[1D   \ESC[1D\ESC[1D\ESC[1D"
  return xs

main :: IO ()
main = run initialEnv
