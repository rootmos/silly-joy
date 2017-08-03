{-# LANGUAGE FlexibleContexts, TypeOperators, TypeFamilies #-}
module Runner ( State (..)
              , simulateUnsafe
              , expect
              , send
              , repl
              ) where

import qualified Data.Map.Lazy as M
import Control.Eff hiding ( send )
import qualified Control.Eff as E
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Exception ( Exception, throw )
import Prelude hiding ( lookup, print )
import Data.Foldable (traverse_)
import Data.Typeable
import Data.List (intercalate)
import Data.OpenUnion (weaken)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

import Parser
import Meaning

initialState :: State
initialState = State { stack = []
                     , dict = primitives
                     , prevState = Nothing
                     }

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
    let p = meaning parsed in
    let (st, ()) = either throw id . run .
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
                  let p = meaning parsed
                  x <- runLift . runRealWorld . runExc . runStateEffect s .
                      unProgram $ p
                  case x of
                    Right (s', ()) -> loop s'
                    Left e -> (putStrLn . show $ e) >> loop s
              Left e -> putStrLn e >> loop s
