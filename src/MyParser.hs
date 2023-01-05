module MyParser where

import Control.Applicative (Alternative (empty, many, some), (<|>))
import Data.Char (chr, isSpace)

newtype Parser a = Parser { run :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser parseFunc) = Parser $ \input -> do
        (input', parsedValue) <- parseFunc input
        Just (input', f parsedValue)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser parseFunc1) <*> (Parser parseFunc2) = Parser $ \input -> do
        (input', f) <- parseFunc1 input
        (input'', a) <- parseFunc2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser parseFunc1) <|> (Parser parseFunc2) = Parser $ \input ->
        parseFunc1 input <|> parseFunc2 input

notNull :: Foldable t => Parser (t a) -> Parser (t a)
notNull (Parser parser) = Parser $ \input -> do
    (input', xs) <- parser input
    if null xs then Nothing else Just (input', xs)

charP :: Char -> Parser Char
charP char = Parser f where
    f (x:xs)
        | x == char = Just (xs, char)
        | otherwise = Nothing
    f [] = Nothing

strP :: String -> Parser String
strP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP condition = Parser $ \input ->
    let (parsedValue, rest) = span condition input
    in Just (rest, parsedValue)

charPredicateP :: (Char -> Bool) -> Parser Char
charPredicateP condition = Parser $ \input ->
    if null input
    then Nothing
    else if condition (head input)
        then Just (tail input, head input)
        else Nothing

nextP :: Parser Char
nextP = Parser f where
    f "" = Nothing
    f (c:s) = Just (s, c)

passP :: Parser ()
passP = Parser f where
    f input = Just (input, ())

holdP :: Parser ()
holdP = Parser f where
    f s = Just (s, ())

whiteSpaces :: Parser String
whiteSpaces = spanP isSpace

manySpaceP :: Parser String
manySpaceP = many (charP ' ' <|> charP '\t')

lineBreak :: Parser String
lineBreak = strP "\r\n" <|> strP "\n"
