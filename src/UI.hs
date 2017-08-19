{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module UI where

import System.Console.Haskeline
import qualified System.Console.Haskeline.Brick as HB

import Brick
import Brick.BChan
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V

import Parser
import Meaning
import qualified Runner as R

import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import qualified Data.Map.Lazy as M

import Control.Monad (void)
import Control.Monad.State.Lazy
import Control.Concurrent (forkFinally)

import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Directory (getAppUserDataDirectory)

import Data.Maybe (fromMaybe)

data Event = StateUpdated R.State
           | FromHBWidget HB.ToBrick
           | HaskelineDied (Either SomeException ())

data AppName = TheApp | HaskelineWidget | StackViewport
    deriving (Ord, Eq, Show)

data AppState = AppState { haskelineWidget :: HB.Widget AppName
                         , joyState :: Maybe R.State
                         }

initialAppState :: AppState
initialAppState =
    AppState { haskelineWidget = HB.initialWidget HaskelineWidget
             , joyState = Nothing
             }

app :: HB.Config Event -> App AppState Event AppName
app c = App { appDraw = drawUI
            , appChooseCursor = \_ -> showCursorNamed HaskelineWidget
            , appHandleEvent = handleEvent c
            , appStartEvent = return
            , appAttrMap = const theMap
            }

handleEvent :: HB.Config Event
            -> AppState -> BrickEvent AppName Event
            -> EventM AppName (Next AppState)
handleEvent c s@AppState{haskelineWidget = hw} e = do
    hw' <- HB.handleEvent c hw e
    handleAppEvent (s { haskelineWidget = hw' }) e

handleAppEvent :: AppState -> BrickEvent AppName Event
               -> EventM AppName (Next AppState)
handleAppEvent s (AppEvent (HaskelineDied e)) = halt s
handleAppEvent s (AppEvent (StateUpdated st)) = continue $
    s { joyState = Just st }
handleAppEvent s _ = continue s

drawUI :: AppState -> [Widget AppName]
drawUI s = [HB.render (haskelineWidget s) <+> stackWidget]
    where
        stackWidget =
            B.border $ hLimit 20 $ viewport StackViewport Vertical $
                vBox $ (map $ str . show) $
                    fromMaybe [] (R.stack <$> joyState s)

theMap :: AttrMap
theMap = attrMap V.defAttr []

runTui :: IO ()
runTui = do
    chan <- newBChan 10
    config <- HB.configure
            chan
            FromHBWidget
            (\case { FromHBWidget x -> Just x; _ -> Nothing })

    _ <- forkFinally
        (runTuiInputT config chan)
        (writeBChan chan . HaskelineDied)
    void $ customMain
        (V.mkVty V.defaultConfig)
        (Just chan)
        (app config)
        initialAppState

runTuiInputT :: HB.Config Event -> BChan Event -> IO ()
runTuiInputT c chan = do
    hs <- haskelineSettings
    (flip evalStateT) R.initialState . runInputTBehavior (HB.useBrick c) hs $ loop
    where
        loop :: InputT (StateT R.State IO) ()
        loop = do
            minput <- getInputLine "> "
            case minput of
              Just unparsed -> doParsing unparsed
              Nothing -> return ()

        doParsing :: String -> InputT (StateT R.State IO) ()
        doParsing unparsed =
            case parse unparsed of
              Right parsed -> do
                  s <- lift get
                  x <- R.runInputT s . meaning $ parsed
                  case x of
                    Right (s', ()) -> do
                        lift (put s')
                        liftIO $ writeBChan chan $ StateUpdated s'
                        loop
                    Left e -> (outputStrLn $ show e) >> loop
              Left e -> outputStrLn e >> loop

runRepl :: IO ()
runRepl = do
    hs <- haskelineSettings
    (flip evalStateT) R.initialState . runInputT hs $ loop
    where
        loop :: InputT (StateT R.State IO) ()
        loop = do
            minput <- getInputLine "> "
            case minput of
              Just (':' : 's' : _) -> dumpStack
              Just unparsed -> doParsing unparsed
              Nothing -> return ()

        dumpStack = do
            s <- lift get
            traverse_ (outputStrLn . show) (R.stack s)
            loop

        doParsing :: String -> InputT (StateT R.State IO) ()
        doParsing unparsed =
            case parse unparsed of
              Right parsed -> do
                  s <- lift get
                  x <- R.runInputT s . meaning $ parsed
                  case x of
                    Right (s', ()) -> lift (put s') >> loop
                    Left e -> (outputStrLn $ show e) >> loop
              Left e -> (outputStrLn e) >> loop


-- Leverage the MonadException from haskeline and the StateT
-- from mtl.
-- This piece of code is taken verbatim from here:
-- https://hackage.haskell.org/package/haskeline-0.7.4.0/docs/src/System.Console.Haskeline.MonadException.html#line-152
instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \run ->
                    fmap (flip runStateT s) $ f $ stateRunIO s run
      where
        stateRunIO :: s -> RunIO m -> RunIO (StateT s m)
        stateRunIO s (RunIO run) = RunIO (\m -> fmap (StateT . const)
                                        $ run (runStateT m s))

haskelineSettings :: (MonadState R.State m, MonadIO m)
                  => IO (Settings m)
haskelineSettings = do
    hf <- getAppUserDataDirectory "silly-joy.history"
    return $ Settings { historyFile = Just hf
                      , complete = completer
                      , autoAddHistory = True
                      }
        where completer = completeWord Nothing [' ', '\t'] $ \w -> do
                            s <- get
                            return $ map simpleCompletion
                                   $ filter (isPrefixOf w) . M.keys . R.dict
                                   $ s
