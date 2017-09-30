module Parser
(
  parseExprs
) where

import Types
import Text.Read
import Data.Char
import Data.Maybe
import Control.Monad
import Text.Parsec
import Data.Functor.Identity

parseExprs :: String -> (SObj, String)
parseExprs input = case parse chunk "" input of
                     Right x -> x
                     _       -> (Nil, "") -- error

chunk :: ParsecT String u Identity (SObj, String)
chunk = do
  first <- spaces >> parseExpr
  rest <- getInput
  return (first, rest)

parseExpr :: ParsecT String u Identity SObj
parseExpr = parseQuote <|> parseAtom <|> parseList

parseList :: ParsecT String u Identity SObj
parseList = try parseNil <|> do
  char '('
  spaces
  results <- sepEndBy parseExpr spaces
  tail <- parseTail
  return $ SList results tail

parseNil :: ParsecT String u Identity SObj
parseNil = do
  string "()"
  return Nil

parseTail :: ParsecT String u Identity SObj
parseTail = do
  last <- char ')' <|> char '.'
  case last of
    ')' -> return Nil
    '.' -> do
      tail <- spaces >> parseExpr
      spaces >> char ')'
      return tail

parseQuote :: ParsecT String u Identity SObj
parseQuote = do
  char '\''
  quoted <- parseExpr
  return $ SList [SSymbol "quote", quoted] Nil

parseAtom :: ParsecT String u Identity SObj
parseAtom = parseInteger <|> parseBool <|> parseSymbol

parseInteger :: ParsecT String u Identity SObj
parseInteger = do
  result <- many1 digit
  let integer = read result
  return $ SInt integer

parseBool :: ParsecT String u Identity SObj
parseBool = do
  result <- char '#' >> (char 't' <|> char 'f')
  case result of
    't' -> return $ SBool True
    'f' -> return $ SBool False

symbolChars :: String
symbolChars = "~!#@$%^&*-_=+:/?<>"

parseSymbol :: ParsecT String u Identity SObj
parseSymbol = do
  first <- letter <|> oneOf symbolChars
  rest <- many (letter <|> digit <|> oneOf symbolChars)
  return $ SSymbol (first:rest)
