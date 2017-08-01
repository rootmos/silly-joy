module Lib ( parse
           , Term (..)
           , AST
           ) where

import Text.Parsec ( many1
                   , alphaNum
                   , Parsec
                   , (<|>)
                   , between
                   , spaces
                   , sepEndBy
                   )
import qualified Text.Parsec as P
import Text.Parsec.Char ( char )
import Data.Bifunctor

data Term = Word String | Quoted AST
    deriving (Eq, Show)

type AST = [Term]

parse :: String -> Either String AST
parse = first show . P.parse ast "parsing silly-joy"

ast :: Parsec String st AST
ast = spaces >> term `sepEndBy` spaces

term :: Parsec String st Term
term = word <|> quoted

word :: Parsec String st Term
word = Word <$> many1 alphaNum

quoted :: Parsec String st Term
quoted = Quoted <$> between (char '[') (char ']') ast
