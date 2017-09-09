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
          (val, newEnv) = evl expr env
      print val
      case rest of
        [] -> run newEnv
        _  -> do newEnv' <- evalRestPrint (rest, newEnv)
                 run newEnv'

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

evalRestPrint :: ([String], Env) -> IO Env
evalRestPrint (x, env) = do
  let (expr, rest)  = parseTokens x
      (val, newEnv) = evl expr env
  print val
  case rest of
    [] -> return newEnv
    _  -> evalRestPrint (rest, newEnv)

main :: IO ()
main = run initialEnv
