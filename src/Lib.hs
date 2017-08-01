{-# LANGUAGE FlexibleContexts, DeriveFunctor, TypeOperators #-}
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
import qualified Data.Map.Lazy as M
import Control.Eff
import Control.Eff.Exception
import Data.Void
import Prelude hiding ( lookup )

data Term = Word Name | Quoted AST
    deriving (Eq, Show)
type Name = String
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


data Value = P Program | I Int

data StateEffect v = Push Value v | Pop (Value -> v) | Lookup Name (Program -> v)
    deriving ( Functor )

data RealWorldEffect v = Print Value v
    deriving ( Functor )

data Error = Undefined Name | PoppingEmptyStack | TypeMismatch

type Program = Eff (StateEffect :> RealWorldEffect :> (Exc Error) :> Void) ()

data State = State { stack :: [Value]
                   , dict :: M.Map Name Program
                   }

lookup :: Member StateEffect e => Name -> Eff e Program
lookup n = send . inj $ Lookup n id

push :: Member StateEffect e => Value -> Eff e ()
push v = send . inj $ Push v ()

pop :: Member StateEffect e => Eff e Value
pop = send . inj $ Pop id

print :: Member RealWorldEffect e => Value -> Eff e ()
print v = send . inj $ Print v ()

initialDictionary :: M.Map Name Program
initialDictionary = M.fromList
    [ ("pop", pop >> return ())
    , ("i", pop >>= castProgram >>= id)
    , ("dup", do v <- pop; push v; push v)
    , ("dip", do v <- pop; pop >>= castProgram >>= id; push v)
    , ("+", do a <- pop >>= castInt; b <- pop >>= castInt; push (I $ a + b))
    ]

castProgram :: Member (Exc Error) e => Value -> Eff e Program
castProgram (P p) = return p
castProgram _ = throwExc TypeMismatch

castInt :: Member (Exc Error) e => Value -> Eff e Int
castInt (I i) = return i
castInt _ = throwExc TypeMismatch

interpret :: AST -> Program
interpret [] = return ()
interpret ((Word n):ts) = lookup n >>= id >> interpret ts
interpret ((Quoted ast):ts) = push (P $ interpret ast) >> interpret ts
