{-# LANGUAGE FlexibleContexts, DeriveFunctor, TypeOperators, TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.List (intercalate)

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

data Value = A [Value] | P Program | I Integer | B Bool | S String
    deriving ( Eq )

instance Show Value where
    show (I i) = show i
    show (B True) = "true"
    show (B False) = "false"
    show (A xs) = "[" ++ (intercalate " " $ map show xs) ++ "]"
    show (P p) = show p
    show (S s) = "\"" ++ s ++ "\""

data Error = Undefined Name
           | PoppingEmptyStack
           | PeekingEmptyStack
           | PoppingEmptyStateStack
           | TypeMismatch Value String
           | UnparseableAsNumber String
           | EmptyAggregate
    deriving ( Show, Eq )

instance Exception Error


-- The StateEffect

data StateEffect v = Push Value v
                   | Pop (Value -> v)
                   | Peek (Value -> v)
                   | Lookup Name (Program -> v)
                   | ClearStack v
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

peek :: Member StateEffect e => Eff e Value
peek = E.send . inj $ Peek id

local :: Member StateEffect e => Eff e a -> Eff e a
local p = do
    () <- E.send . inj $ PushState ()
    a <- p
    () <- E.send . inj $ PopState ()
    return a

bind :: Member StateEffect e => Name -> Program -> Eff e ()
bind n p = E.send . inj $ Bind n p ()

clear :: Member StateEffect e => Eff e ()
clear = E.send . inj $ ClearStack ()

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
    [ mk "pop" $ pop >> return ()
    , mk "clear" $ clear
    , mk "i" $ pop >>= castProgram >>= unProgram
    , mk "x" $ peek >>= castProgram >>= unProgram
    , mk "dup" $ peek >>= push
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
    , mk "true" $ push (B True)
    , mk "false" $ push (B False)
    , mk "and" $ do
        a <- pop >>= castBool
        b <- pop >>= castBool
        push (B $ a && b)
    , mk "or" $ do
        a <- pop >>= castBool
        b <- pop >>= castBool
        push (B $ a || b)
    , mk "not" $ do
        a <- pop >>= castBool
        push (B $ not a)
    , mk "null" $ do
        v <- pop
        case v of
          I 0 -> push $ B True
          I _ -> push $ B False
          A [] -> push $ B True
          A _ -> push $ B False
          _ -> throwExc $ TypeMismatch v "expecting number or aggregate"
    , mk "small" $ do
        v <- pop
        case v of
          I 0 -> push $ B True
          I 1 -> push $ B True
          I _ -> push $ B False
          A [] -> push $ B True
          A (_:[]) -> push $ B True
          A _ -> push $ B False
          _ -> throwExc $ TypeMismatch v "expecting number or aggregate"
    , mk "succ" $ do a <- pop >>= castInt; push (I $ succ a)
    , mk "pred" $ do a <- pop >>= castInt; push (I $ pred a)
    , mk "/" $ do
        a <- pop >>= castInt
        b <- pop >>= castInt
        push (I $ b `div` a)
    , mk "%" $ do
        a <- pop >>= castInt
        b <- pop >>= castInt
        push (I $ b `mod` a)
    , mk "div" $ do
        a <- pop >>= castInt
        b <- pop >>= castInt
        let (q, r) = divMod b a
        push (I q)
        push (I r)
    , mk "print" $ pop >>= print . show
    , mk "ifte" $ do
        false <- pop
        true <- pop
        cond <- pop

        c <- local (castProgram cond >>= unProgram >> peek >>= castBool)
        case c of
          True -> castProgram true >>= unProgram
          False -> castProgram false >>= unProgram
    , mk "I" $ do
        p <- pop >>= castProgram
        v <- local (unProgram p >> peek)
        push v
    , mk "swap" $ do a <- pop; b <- pop; push a; push b
    , mk "concat" $ do
        a <- pop >>= castAggregate
        b <- pop >>= castAggregate
        push $ A (b <> a)
    , mk "b" $ do
        a <- pop >>= castProgram
        b <- pop >>= castProgram
        (unProgram b) >> (unProgram a)
    , mk "size" $ do
        a <- pop >>= castAggregate
        push $ I (toInteger $ length a)
    , mk "cons" $ do
        a <- pop >>= castAggregate
        b <- pop
        push $ A (b : a)
    , mk "first" $ do
        ag <- pop >>= castAggregate
        case ag of
          (P p):_ -> unProgram p
          v:_ -> push v
          [] -> throwExc EmptyAggregate
    , mk "rest" $ do
        ag <- pop >>= castAggregate
        case ag of
          _:tl -> push $ A tl
          [] -> throwExc EmptyAggregate
    , mk "uncons" $ do
        ag <- pop >>= castAggregate
        case ag of
          (P p):tl -> do
              unProgram p
              push $ A tl
          v:tl -> do
              push v
              push $ A tl
          [] -> throwExc EmptyAggregate
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
    , mk "rolldown" $ do
        z <- pop
        y <- pop
        x <- pop
        push y
        push z
        push x
    , mk "rollup" $ do
        z <- pop
        y <- pop
        x <- pop
        push z
        push x
        push y
    , mk "rotate" $ do
        z <- pop
        y <- pop
        x <- pop
        push z
        push y
        push x
    , mk "primrec" $ do
        c <- pop >>= castProgram
        i <- pop >>= castProgram
        let loop = do
                x <- peek >>= castInt
                case x of
                  0 -> pop >> unProgram i
                  n -> push (I $ n - 1) >> loop >> unProgram c
        loop
    , mk "linrec" $ do
        r2 <- pop >>= castProgram
        r1 <- pop >>= castProgram
        t <- pop >>= castProgram
        p <- pop >>= castProgram
        let loop = do
                b <- local (unProgram p >> peek >>= castBool)
                if b then unProgram t
                     else unProgram r1 >> loop >> unProgram r2
        loop
    , mk "genrec" $ do
        let loop = do
                r2 <- pop
                r1 <- pop
                t <- pop
                b <- pop
                b' <- local (castProgram b >>= unProgram >> peek >>= castBool)
                if b' then castProgram t >>= unProgram
                      else do
                          castProgram r1 >>= unProgram
                          push $ A [b, t, r1, r2, P $ MkProgram loop []]
                          castProgram r2 >>= unProgram
        loop
    , mk "times" $ do
        n <- pop >>= castInt
        p <- pop >>= castProgram
        sequence_ $ replicate (fromInteger n) $ unProgram p
    , mk "map" $ do
        p <- pop >>= castProgram
        as <- pop >>= castAggregate
        as' <- sequence $ flip fmap as $
            \case { P q -> local $ unProgram q >> unProgram p >> peek;
                    v -> local $ push v >> unProgram p >> peek
                  }
        push $ A as'
    , mk "filter" $ do
        p <- pop >>= castProgram
        as <- pop >>= castAggregate
        as' <- sequence $ flip fmap as $ \v -> local $ do
            case v of
              P q -> unProgram q
              _ -> push v
            unProgram p
            b <- peek >>= castBool
            return $ if b then [v] else []
        push $ A $ concat as'
    , mk "fold" $ do
        f <- pop >>= castProgram
        z <- pop
        as <- pop >>= castAggregate
        let go = \case { [] -> return ();
                         (P p:tl) -> unProgram p >> unProgram f >> go tl;
                         (v:tl) -> push v >> unProgram f >> go tl
                       }
        push z
        go as
    , mk "app2" $ do
        f <- pop >>= castProgram
        b <- pop
        a <- pop
        b' <- local (push b >> unProgram f >> peek)
        a' <- local (push a >> unProgram f >> peek)
        push a'
        push b'
    ]
        where
            mk n p = (n, MkProgram { unProgram = p, ast = [Word n] })

            castProgram :: Member (Exc Error) e => Value -> Eff e Program
            castProgram (P p) = return p
            castProgram (A xs) =
                sequence (map weakCastProgram xs) >>= return . mconcat
                    where
                        weakCastProgram :: Member (Exc Error) e
                                        => Value -> Eff e Program
                        weakCastProgram (I n) = return $
                            MkProgram { unProgram = push (I n)
                                      , ast = [Number n]
                                      }
                        weakCastProgram (S s) = return $
                            MkProgram { unProgram = push (S s)
                                      , ast = [Str s]
                                      }
                        weakCastProgram (A xs) = return $
                            MkProgram { unProgram = push $ A xs
                                      , ast = [] -- TODO: Are any casted programs shown to the user?
                                      }
                        weakCastProgram v = castProgram v
            castProgram v = throwExc $ TypeMismatch v "expecting program"

            castAggregate :: Member (Exc Error) e => Value -> Eff e [Value]
            castAggregate (A xs) = return xs
            castAggregate v = throwExc $ TypeMismatch v "expecting aggregate"

            castInt :: Member (Exc Error) e => Value -> Eff e Integer
            castInt (I i) = return i
            castInt v = throwExc $ TypeMismatch v "expecting number"

            castBool :: Member (Exc Error) e => Value -> Eff e Bool
            castBool (B b) = return b
            castBool v = throwExc $ TypeMismatch v "expecting boolean"

            castStr :: Member (Exc Error) e => Value -> Eff e String
            castStr (S s) = return s
            castStr v = throwExc $ TypeMismatch v "expecting string"

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
meaning (a@(Quoted as):ts) =
    let p = MkProgram { unProgram = do
                            push (A $ map (\t -> P . meaning $ [t]) as)
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
