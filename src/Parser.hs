module Parser ( parse
              , Term (..)
              , Name
              , AST
              , prettyAST
              ) where

import Text.Parsec ( many1
                   , alphaNum
                   , Parsec
                   , (<|>)
                   , between
                   , spaces
                   , space
                   , optional
                   , sepEndBy
                   , many
                   , eof
                   , oneOf
                   , try
                   )
import qualified Text.Parsec as P
import Text.Parsec.Char (char, letter, digit)
import Data.List (intercalate)
import Data.Bifunctor (first)

data Term = Word Name
          | Quoted AST
          | Number Integer
          | Str String
          | Binding Name AST
    deriving (Eq, Show)
type Name = String
type AST = [Term]

prettyAST :: AST -> String
prettyAST = intercalate " " . map prettyTerm
    where
        prettyTerm (Word n) = n
        prettyTerm (Number n) = show n
        prettyTerm (Quoted a) = "[" ++ prettyAST a ++ "]"
        prettyTerm (Str s) = "\"" ++ s ++ "\""
        prettyTerm (Binding n a) = n ++ " := " ++ prettyAST a ++ ";"

parse :: String -> Either String AST
parse = first show . P.parse (ast <* eof) "parsing silly-joy"

ast :: Parsec String st AST
ast = spaces >> (try binding <|> term) `sepEndBy` separator
    where
        separator = optional spaces >> optional (char ';') >> spaces

binding :: Parsec String st Term
binding = do
    n <- name
    _ <- spaces
    _ <- char ':'
    _ <- char '='
    _ <- spaces
    a <- term `sepEndBy` spaces
    return $ Binding n a
    where
        name = many1 $ alphaNum <|> oneOf symbols

symbols :: [Char]
symbols = ['+', '=', '<', '>', '!', '-', '*', '_', '/', '%']

term :: Parsec String st Term
term = number <|> word <|> quoted <|> str

str :: Parsec String st Term
str = Str <$> between (char '"') (char '"') (many quoted_char)
    where
        quoted_char = space
               <|> alphaNum
               <|> oneOf symbols
               <|> (try (char '\\') >> char '"')

word :: Parsec String st Term
word = do
    l <- letter <|> symbol
    ans <- many (alphaNum <|> symbol)
    return . Word $ l : ans
        where symbol = oneOf symbols

number :: Parsec String st Term
number = positive <|> try negative
    where
        positive = Number . read <$> many1 digit
        negative = do
            _ <- char '-'
            n <- many1 digit
            return . Number $ -1 * read n

quoted :: Parsec String st Term
quoted = Quoted <$> between (char '[') (char ']') ast
