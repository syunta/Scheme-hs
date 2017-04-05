import Parser
import Evaluator

run :: IO ()
run = do
  x <- readPrompt
  let (expr, rest) = parse x
      (val, env)   = eval (expr, initialEnv)
  putStrLn (show val)
  run

readPrompt :: IO String
readPrompt = do
  putStr "scheme>"
  getLine

main :: IO ()
main = run
