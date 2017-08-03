{-# LANGUAGE FlexibleContexts, DeriveFunctor, TypeOperators, TypeFamilies, TypeSynonymInstances #-}
module Lib ( State (..)
           , simulateUnsafe
           , expect
           , send
           , Value (..)
           , Error (..)
           , repl
           ) where

import qualified Data.Map.Lazy as M
import Control.Eff hiding ( send, run )
import qualified Control.Eff as E
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Exception ( Exception, throw )
import Data.Void
import Prelude hiding ( lookup, print )
import Data.Foldable (traverse_)
import Data.Typeable
import Data.List (intercalate)
import Data.OpenUnion (weaken)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Text.Read (readMaybe)
import Data.Monoid

import Parser


data Value = P Program | I Integer | B Bool | S String
    deriving ( Eq )

instance Show Value where
    show (I i) = show i
    show (B b) = show b
    show (P p) = "[" ++ show p ++ "]"
    show (S s) = "\"" ++ s ++ "\""

data StateEffect v = Push Value v
                   | Pop (Value -> v)
                   | Lookup Name (Program -> v)
                   | PushState v
                   | PopState v
                   | Bind Name Program v
    deriving ( Functor )

data RealWorldEffect v = Print String v
                       | Input (String -> v)
    -- TODO: Maybe extend Input Int to Input Value and interpret string
    deriving ( Functor )

data Error = Undefined Name
           | PoppingEmptyStack
           | PoppingEmptyStateStack
           | TypeMismatch
           | UnparseableAsNumber String
    deriving ( Show, Eq )

instance Exception Error

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

data State = State { stack :: [Value]
                   , dict :: M.Map Name Program
                   , prevState :: Maybe State
                   }

instance Show State where
    show (State { stack = s, prevState = ps }) =
        "stack=" ++ intercalate "," (map show s) ++ prettyPrevState ps
            where
                prettyPrevState (Just st) = " prevState={" ++ show st ++ "}"
                prettyPrevState Nothing = " prevState={}"

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

print :: Member RealWorldEffect e => String -> Eff e ()
print s = E.send . inj $ Print s ()

input :: Member RealWorldEffect e => Eff e String
input = E.send . inj $ Input id

initialDictionary :: M.Map Name Program
initialDictionary = M.fromList
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

initialState :: State
initialState = State { stack = []
                     , dict = initialDictionary
                     , prevState = Nothing
                     }

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

interpret :: AST -> Program
interpret [] = mempty
interpret (a@(Word n):ts) =
    let p = MkProgram { unProgram = lookup n >>= unProgram
                      , ast = [a]
                      } in
    p <> interpret ts
interpret (a@(Quoted q):ts) =
    let p = MkProgram { unProgram = push (P $ interpret q)
                      , ast = [a]
                      } in
    p <> interpret ts
interpret (a@(Number n):ts) =
    let p = MkProgram { unProgram = push (I n), ast = [a] } in
    p <> interpret ts
interpret (a@(Str s):ts) =
    let p = MkProgram { unProgram = push (S s), ast = [a] } in
    p <> interpret ts
interpret (a@(Binding n b):ts) =
    let p = MkProgram { unProgram = do
                            push $ P (interpret b)
                            push $ S n
                            lookup "bind" >>= unProgram
                      , ast = [a] } in
    p <> interpret ts

runStateEffect :: Member (Exc Error) e => State -> Eff (StateEffect :> e) v
               -> Eff e (State, v)
runStateEffect st = freeMap (\x -> return (st, x))
    (\u -> handleRelay u (runStateEffect st) (handle st))
        where
        handle s (Push v k) =
            let s' = s { stack = v : stack s } in runStateEffect s' k

        handle (s@State { stack = v : stk }) (Pop k) =
            runStateEffect (s { stack = stk }) (k v)

        handle (State { stack = [] }) (Pop _) =
            throwExc PoppingEmptyStack

        handle (s@State { dict = d }) (Lookup n k) =
            case M.lookup n d of
              Just p -> runStateEffect s (k p)
              Nothing -> throwExc $ Undefined n

        handle s (PushState k) = do
            let s' = s { prevState = Just s }
            runStateEffect s' k

        handle (State { prevState = Just s }) (PopState k) =
            runStateEffect s k

        handle (State { prevState = Nothing }) (PopState _) =
            throwExc PoppingEmptyStateStack

        handle (s@State { dict = d }) (Bind n p k) = do
            let s' = s { dict = M.insert n p d }
            runStateEffect s' k


data SimulatedIO = ExpectOutput String | SendInput String

send :: String -> SimulatedIO
send = SendInput

expect :: String -> SimulatedIO
expect = ExpectOutput

data SimulatedIOError = UnexpectedOutput String
                      | IncorrectOutput String String
                      | UnexpectedInput
                      | EndOfExpectations
                      deriving ( Show )

instance Exception SimulatedIOError

simulateRealWorld :: [SimulatedIO] -> Eff (RealWorldEffect :> e) v -> Eff e v
simulateRealWorld expects = freeMap return
    (\u -> handleRelay u (simulateRealWorld expects) (handle expects))
        where
            handle [] _ = throw EndOfExpectations
            handle (ExpectOutput o : exs) (Print o' k)
              | o == o' = simulateRealWorld exs k
              | otherwise = throw $ IncorrectOutput o o'

            handle (SendInput i : exs) (Input k) =
                simulateRealWorld exs (k i)

            handle (SendInput _ : _) (Print o _) =
                throw $ UnexpectedOutput o

            handle (ExpectOutput _ : _) (Input _) =
                throw $ UnexpectedInput

simulateUnsafe :: String -> [SimulatedIO] -> State
simulateUnsafe s exs =
    let parsed = either error id $ parse s in
    let p = interpret parsed in
    let (st, ()) = either throw id . E.run .
                    simulateRealWorld exs . runExc .
                        runStateEffect initialState . unProgram $ p in
        st

runRealWorld :: Eff (RealWorldEffect :> e) v -> Eff (Lift IO :> e) v
runRealWorld = freeMap return (\u -> transform u runRealWorld handle)
    where
        handle :: RealWorldEffect (Eff (RealWorldEffect :> e) w)
               -> Eff (Lift IO :> e) w
        handle (Print s k) = lift (putStrLn s) >> runRealWorld k
        handle (Input k) = lift getLine >>= runRealWorld . k

transform :: (Typeable t, Typeable s, Functor s)
          => Union (t :> r) v -- ^ Request
          -> (v -> Eff (s :> r) a)   -- ^ Relay the request
          -> (t v -> Eff (s :> r) a) -- ^ Handle the request of type t
          -> Eff (s :> r) a
transform u loop h = either passOn h $ decomp u
    where
        passOn u' = E.send (weaken u') >>= loop

repl :: IO ()
repl = do
    hSetBuffering stdout NoBuffering
    loop initialState
    where
        loop s = do
            putStr "> "
            raw <- getLine
            case raw of
              ':' : 's' : _ -> dumpStack s
              _ -> doParsing raw s

        dumpStack s =
            traverse_ (putStrLn . show) (stack s) >> loop s

        doParsing raw s =
            case parse raw of
              Right parsed -> do
                  let p = interpret parsed
                  x <- runLift . runRealWorld . runExc . runStateEffect s .
                      unProgram $ p
                  case x of
                    Right (s', ()) -> loop s'
                    Left e -> (putStrLn . show $ e) >> loop s
              Left e -> putStrLn e >> loop s
