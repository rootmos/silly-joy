{-# LANGUAGE FlexibleContexts, DeriveFunctor, TypeOperators, TypeFamilies #-}
module Meaning ( Program (unProgram)
               , StateEffect (..)
               , RealWorldEffect (..)
               , Error (..)
               , Value (..)
               , primitives
               , meaning
               ) where

import qualified Data.Map.Lazy as M
import Control.Eff hiding ( send, run )
import qualified Control.Eff as E
import Control.Eff.Exception
import Control.Exception ( Exception )
import Data.Void
import Prelude hiding ( lookup, print )
import Text.Read (readMaybe)
import Data.Monoid

import Parser

data Program = MkProgram { unProgram :: Eff (StateEffect
                                                :> (Exc Error)
                                                :> RealWorldEffect :> Void) ()
                         , ast :: AST
                         }

instance Monoid Program where
    mempty = MkProgram { unProgram = return (), ast = [] }
    p `mappend` q = MkProgram { unProgram = unProgram p >> unProgram q
                              , ast = ast p ++ ast q
                              }

instance Eq Program where
    p == p' = ast p == ast p'

instance Show Program where
    show = prettyAST . ast


-- Value

data Value = P Program | I Integer | B Bool | S String
    deriving ( Eq )

instance Show Value where
    show (I i) = show i
    show (B b) = show b
    show (P p) = "[" ++ show p ++ "]"
    show (S s) = "\"" ++ s ++ "\""

data Error = Undefined Name
           | PoppingEmptyStack
           | PoppingEmptyStateStack
           | TypeMismatch
           | UnparseableAsNumber String
    deriving ( Show, Eq )

instance Exception Error


-- The StateEffect

data StateEffect v = Push Value v
                   | Pop (Value -> v)
                   | Lookup Name (Program -> v)
                   | PushState v
                   | PopState v
                   | Bind Name Program v
    deriving ( Functor )

lookup :: Member StateEffect e => Name -> Eff e Program
lookup n = E.send . inj $ Lookup n id

push :: Member StateEffect e => Value -> Eff e ()
push v = E.send . inj $ Push v ()

pop :: Member StateEffect e => Eff e Value
pop = E.send . inj $ Pop id

local :: Member StateEffect e => Eff e a -> Eff e a
local p = do
    () <- E.send . inj $ PushState ()
    a <- p
    () <- E.send . inj $ PopState ()
    return a

bind :: Member StateEffect e => Name -> Program -> Eff e ()
bind n p = E.send . inj $ Bind n p ()


-- RealWorldEffect

data RealWorldEffect v = Print String v
                       | Input (String -> v)
    deriving ( Functor )


print :: Member RealWorldEffect e => String -> Eff e ()
print s = E.send . inj $ Print s ()

input :: Member RealWorldEffect e => Eff e String
input = E.send . inj $ Input id


-- The primitives

primitives :: M.Map Name Program
primitives = M.fromList
    [ mk "pop" $ do
        _ <- pop
        return ()
    , mk "i" $ do
        pop >>= castProgram >>= unProgram
    , mk "dup" $ do
        v <- pop
        push v
        push v
    , mk "dip" $ do
        p <- pop
        v <- pop
        castProgram p >>= unProgram
        push v
    , mk "+" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (I $ b + a)
    , mk "-" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (I $ b - a)
    , mk "*" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (I $ b * a)
    , mk "<" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (B $ b < a)
    , mk ">" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (B $ b > a)
    , mk "<=" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (B $ b <= a)
    , mk ">=" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (B $ b >= a)
    , mk "=" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (B $ b == a)
    , mk "!=" $ do a <- pop >>= castInt; b <- pop >>= castInt; push (B $ b /= a)
    , mk "print" $ pop >>= print . show
    , mk "ifte" $ do
        false <- pop
        true <- pop
        cond <- pop

        c <- local (castProgram cond >>= unProgram >> pop >>= castBool)
        case c of
          True -> castProgram true >>= unProgram
          False -> castProgram false >>= unProgram
    , mk "I" $ do
        p <- pop >>= castProgram
        v <- local (unProgram p >> pop)
        push v
    , mk "swap" $ do a <- pop; b <- pop; push a; push b
    , mk "concat" $ do
        a <- pop >>= castProgram
        b <- pop >>= castProgram
        push $ P (b <> a)
    , mk "b" $ do
        a <- pop >>= castProgram
        b <- pop >>= castProgram
        (unProgram b) >> (unProgram a)
    , mk "cons" $ do
        a <- pop >>= castProgram
        b <- pop
        bp <- castProgram b
        push $ P (MkProgram { unProgram = push b >> unProgram a
                            , ast = [Quoted $ ast bp] ++ ast a
                            })
    , mk "strlen" $ do
        s <- pop >>= castStr
        push $ I (toInteger $ length s)
    , mk "strcat" $ do
        s <- pop >>= castStr
        s' <- pop >>= castStr
        push $ S (s' ++ s)
    , mk "bind" $ do
        n <- pop >>= castStr
        p <- pop >>= castProgram
        bind n p
    , mk "read_line" $ do
        s <- input
        push $ S s
    , mk "read_int" $ do
        s <- input
        case readMaybe s of
          Just i -> push $ I i
          Nothing -> throwExc $ UnparseableAsNumber s
    ]
        where
            mk n p = (n, MkProgram { unProgram = p, ast = [Word n] })

            castProgram :: Member (Exc Error) e => Value -> Eff e Program
            castProgram (P p) = return p
            castProgram _ = throwExc TypeMismatch

            castInt :: Member (Exc Error) e => Value -> Eff e Integer
            castInt (I i) = return i
            castInt _ = throwExc TypeMismatch

            castBool :: Member (Exc Error) e => Value -> Eff e Bool
            castBool (B b) = return b
            castBool _ = throwExc TypeMismatch

            castStr :: Member (Exc Error) e => Value -> Eff e String
            castStr (S s) = return s
            castStr _ = throwExc TypeMismatch

-- Meaning function
--  - AST is just [Term]: the syntactic monoid
--  - Program also a monoid, and can be seen as the semantic monoid

meaning :: AST -> Program
meaning [] = mempty
meaning (a@(Word n):ts) =
    let p = MkProgram { unProgram = lookup n >>= unProgram
                      , ast = [a]
                      } in
    p <> meaning ts
meaning (a@(Quoted q):ts) =
    let p = MkProgram { unProgram = push (P $ meaning q)
                      , ast = [a]
                      } in
    p <> meaning ts
meaning (a@(Number n):ts) =
    let p = MkProgram { unProgram = push (I n), ast = [a] } in
    p <> meaning ts
meaning (a@(Str s):ts) =
    let p = MkProgram { unProgram = push (S s), ast = [a] } in
    p <> meaning ts
meaning (a@(Binding n b):ts) =
    let p = MkProgram { unProgram = do
                            push $ P (meaning b)
                            push $ S n
                            lookup "bind" >>= unProgram
                      , ast = [a] } in
    p <> meaning ts
