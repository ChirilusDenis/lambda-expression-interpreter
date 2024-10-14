module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding
import Data.Char (isAlpha, isLower, isUpper, isDigit, isNumber)
import Data.Fixed (HasResolution(resolution))

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

apply (Parser p) = p

instance Applicative Parser where
  af <*> mp = 
    do 
      f <- af
      v <- mp
      return $ f v
  pure = return

instance Functor Parser where 
  fmap f mp = 
    do 
      x <- mp
      return $ f x

-- REMEMBER MONADS
instance Monad Parser where
-- take parser a, apply on s, extract a from inside, apply parser b on the rest of the string, build parser with that
    monadparser >>= f = Parser {
        parse = \s -> case apply monadparser s of
            Nothing -> Nothing
            Just(val, rst) -> apply (f val) rst
    }

    return x = Parser {parse = \s -> Just(x, s)}

instance Alternative Parser where
  empty = failParser
  -- if p1 fails, apply p2
  p1 <|> p2 = Parser $ \s -> case apply p1 s of
                                Nothing -> apply p2 s
                                x -> x

oneOrMore :: (Parser a) -> Parser [a]
oneOrMore p =
  do 
    x <- p
    xs <- zeroOrMore p
    return (x:xs)

zeroOrMore :: (Parser a) -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> (return [])

failParser :: Parser a
failParser = Parser {parse = \s -> Nothing}

charParser :: Char -> Parser Char
charParser c = Parser {
    parse = \s -> case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just(x, xs) else Nothing 
}

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser {
    parse = \s -> case s of
    [] -> Nothing
    (x:xs) -> if p x then Just(x, xs) else Nothing
}

varParser :: Parser String
varParser = do
    x <- oneOrMore (predicateParser isLower)
    return x

varLambda :: Parser Lambda
varLambda = do
    x <- varParser
    return (Var x)

absLambda :: Parser Lambda
absLambda = do
  charParser '\\'
  var <- varParser
  charParser '.'
  lambda <- totalParser
  return (Abs var lambda)

appLambda :: Parser Lambda
appLambda = do
  charParser '('
  l1 <- totalParser
  oneOrMore (charParser ' ')
  l2 <- totalParser
  charParser ')'
  return (App l1 l2)

macroParser :: Parser String
macroParser = do
  x <- oneOrMore (predicateParser (\c -> (||) (isUpper c) (isNumber c)))
  return x

macroLambda :: Parser Lambda
macroLambda = do
  x <- macroParser
  return (Macro x)

totalParser :: Parser Lambda
totalParser = do
  x <- appLambda <|> absLambda <|> varLambda <|> macroLambda
  return x

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda s = case apply totalParser s of
  Just (lambda, _) -> lambda

bindParser :: Parser Line
bindParser = do
  name <- macroParser
  charParser '='
  expr <- totalParser
  return (Binding name expr)

evalParser :: Parser Line
evalParser = do
  expr <- totalParser
  return (Eval expr)

lineParser :: Parser Line
lineParser = do 
  x <- bindParser <|> evalParser
  return x

-- 3.3.
parseLine :: String -> Either String Line
-- if there is stuff left to parse after parse line = one than more expr => error
parseLine s = case apply lineParser s of
  Just (line, rest) -> case rest of
    "" -> Right line
    _ -> Left "ERROR"
  Nothing -> Left "ERROR"
