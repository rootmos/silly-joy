{-# LANGUAGE FlexibleContexts, DeriveFunctor, TypeOperators, TypeFamilies #-}
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
import Prelude hiding ( lookup, print )

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

data RealWorldEffect v = Print String v | Input (String -> v)
    -- TODO: Maybe extend Input Int to Input Value and interpret string
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

print :: Member RealWorldEffect e => String -> Eff e ()
print s = send . inj $ Print s ()

initialDictionary :: M.Map Name Program
initialDictionary = M.fromList
    [ ("pop", pop >> return ())
    , ("i", pop >>= castProgram >>= id)
    , ("dup", do v <- pop; push v; push v)
    , ("dip", do v <- pop; pop >>= castProgram >>= id; push v)
    , ("+", do a <- pop >>= castInt; b <- pop >>= castInt; push (I $ a + b))
    , ("print", pop >>= print . show)
    ]

initialState :: State
initialState = State { stack = [], dict = initialDictionary }

castProgram :: Member (Exc Error) e => Value -> Eff e Program
castProgram (P p) = return p
castProgram _ = throwExc TypeMismatch

castInt :: Member (Exc Error) e => Value -> Eff e Int
castInt (I i) = return i
castInt _ = throwExc TypeMismatch

interpret :: AST -> Program
interpret [] = return ()
interpret ((Word n):ts) = lookup n >>= id >> interpret ts
interpret ((Quoted a):ts) = push (P $ interpret a) >> interpret ts

runStateEffect :: Member (Exc Error) e => Eff (StateEffect :> e) v -> Eff e (State, v)
runStateEffect = loop initialState
    where
        loop s = freeMap
            (\x -> return (s, x))
            (\u -> handleRelay u (loop s) (handle s))

        handle s (Push v k) =
            let s' = s { stack = v : stack s } in loop s' k

        handle (s@State { stack = v : st }) (Pop k) =
            loop (s { stack = st }) (k v)

        handle (State { stack = [] }) (Pop _) =
            throwExc PoppingEmptyStack

        handle (s@State { dict = d }) (Lookup n k) =
            case M.lookup n d of
              Just p -> loop s (k p)
              Nothing -> throwExc $ Undefined n

instance Show Value where
    show (I i) = show i
    show (P _) = "<program>"

data SimulatedIO = ExpectOutput String | SendInput String
data SimulatedIOError = UnexpectedOutput String String
                      | UnexpectedInput
                      | EndOfExpectations

simulateRealWorld :: Member (Exc SimulatedIOError) e
                  => [SimulatedIO] -> Eff (RealWorldEffect :> e) v
                  -> Eff e v
simulateRealWorld exs = freeMap return
    (\u -> handleRelay u (simulateRealWorld exs) (handle exs))
        where
            handle [] _ = throwExc EndOfExpectations
            handle (ExpectOutput o : exs) (Print o' k) 
              | o == o' = simulateRealWorld exs k
              | otherwise = throwExc $ UnexpectedOutput o o'

            handle (SendInput i : exs) (Input k) =
                simulateRealWorld exs (k i)
