{-# LANGUAGE LambdaCase #-}
module Excs13 where

import Control.Applicative
import Data.Char (isDigit, isLower, isUpper, isAlpha, isAlphaNum)
import GHC.Unicode (isSpace)
-- import qualified Control.Monad
-- import Excs9 (exprs)
-- import Excs7 (iterateInTermsOfUnfold)

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
  fmap f pa = P (\inp -> case parse pa inp of
                    [] -> []
                    (a,out):_ -> [(f a, out)])

instance Applicative Parser where
  pure a = P (\inp -> [(a,inp)])
  pab <*> pa = P (\inp -> case parse pab inp of
                       [] -> []
                       (g,out):_ -> parse (fmap g pa) out)

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                            [] -> []
                            (a,out):_ -> parse (f a) out)

instance Alternative Parser where
  empty = P (const [])
  p <|> q = P (\inp -> case parse p inp of
                              [] -> parse q inp
                              other -> other)

parse :: Parser a -> String -> [(a,String)]
parse (P f) = f

item :: Parser Char
item = P (\case
             [] -> []
             (x:xs) -> [(x,xs)]
         )

-- >>> parse three "abcdef"
-- [(('a','c'),"def")]
three :: Parser (Char,Char)
three = g <$> item <*> item <*> item
        where g x _ z = (x,z)


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

int :: Parser Int
int = do _ <- char '-'
         n <- nat
         return (-n)
      <|> nat

-- --------------------

token :: Parser a -> Parser a
token p = do _ <- space
             v <- p
             _ <- space
             return v

identifier :: Parser String
identifier  = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

-- ------------------------------

-- >>> parse comment "-- fdsadf"
-- [((),"")]
comment :: Parser ()
comment = do _ <- string "--"
             _ <- many (sat (/= '\n'))
             return ()

-- 2. 2+3+4

-- expr ::= expr + expr | term
-- term ::= term * term | factor
-- factor ::= ( expr ) | nat
-- nat ::= 0 | 1 | 2 | ...

--            expr
--     term    +          expr
--     factor        term  + term
--     nat           factor  factor
--      2            nat     nat
--                   3       4


-- second tree is the same like the one above except
-- that two branches switched

-- 3.
-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- factor ::= ( expr ) | nat
-- nat ::= 0 | 1 | 2 | ...

-- 2 + 3
--            expr
--      term   +    expr
--      factor      term
--      nat         factor
--      2           nat
--                  3

-- 2*3*4
--             term
--      factor  *        term
--      nat        factor  *   term
--      2          nat         factor
--                 3           nat
--                             4


-- (2+3)+4
--                              expr
--                     term      +      expr
--                                      term
--              factor                  factor
--               expr                   nat
--       term     +    expr             4
--       factor        term
--       nat           factor
--       2             nat
--                     3




-- expr ::= term (+ expr | - expr | e)
-- term ::= expo (* term | / term | e)
-- expo ::= factor (^ factor | e) 
-- factor ::= ( expr ) | nat
-- nat ::= 0 | 1 | 2 | ...
data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Exp Expr Expr | Nat Int
            deriving (Show)

-- >>> show "fdsfsdadfas"
-- "\"fdsfsdadfas\""

-- >>> parse expr "2-2^2"
-- [(Sub (Nat 2) (Exp (Nat 2) (Nat 2)),"")]

-- >>> parse expr "2^2*3"
-- [(Exp (Nat 2) (Mul (Nat 2) (Nat 3)),"")]

expr :: Parser Expr
expr = do t <- term
          symbol "+" *> (Add t <$> expr) <|> symbol "-" *> (Sub t <$> expr)
           <|> return t

term :: Parser Expr
term = do f <- expo
          symbol "*" *> (Mul f <$> term) <|> symbol "/" *> (Div f <$> expr)
           <|> return f

expo :: Parser Expr
expo = do e <- factor
          symbol "^" *> (Exp e <$> factor)
           <|> return e

factor :: Parser Expr
factor = do _ <- symbol "("
            e <- expr
            _ <- symbol ")"
            return e
          <|> Nat <$> natural

-- 

-- 8 ----------------------------------------

-- grammar
-- expr ::= expr - nat | nat


-- infinite recursion
expr8 :: Parser Expr
expr8 = do e <- expr8
           _ <- symbol "-"
           n <- Nat <$> natural
           return (Sub e n)
        <|> Nat <$> natural

-- >>> parse expr8Fixed "9-1-3"
-- [(5,"")]

expr8Fixed :: Parser Int
expr8Fixed = do first <- natural
                others <- many $ symbol "-" *>  natural
                return $ foldl (-) first others


