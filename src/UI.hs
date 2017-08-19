{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (void)
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
    runInputTBehavior (HB.useBrick c) hs $ loop R.initialState
    where
        loop s = do
            minput <- getInputLine "> "
            case minput of
              Just unparsed -> doParsing unparsed s
              Nothing -> return ()

        doParsing unparsed s =
            case parse unparsed of
              Right parsed -> do
                  x <- R.runInputT s . meaning $ parsed
                  case x of
                    Right (s', ()) -> do
                        liftIO $ writeBChan chan $ StateUpdated s'
                        loop s'
                    Left e -> (outputStrLn $ show e) >> loop s
              Left e -> outputStrLn e >> loop s

runRepl :: IO ()
runRepl = do
    hs <- haskelineSettings
    runInputT hs $ loop R.initialState
    where
        loop s = do
            minput <- getInputLine "> "
            case minput of
              Just (':' : 's' : _) -> dumpStack s
              Just unparsed -> doParsing unparsed s
              Nothing -> return ()

        dumpStack s =
            traverse_ (outputStrLn . show) (R.stack s) >> loop s

        doParsing unparsed s =
            case parse unparsed of
              Right parsed -> do
                  x <- R.runInputT s . meaning $ parsed
                  case x of
                    Right (s', ()) -> loop s'
                    Left e -> (outputStrLn $ show e) >> loop s
              Left e -> outputStrLn e >> loop s

haskelineSettings :: MonadIO m => IO (Settings m)
haskelineSettings = do
    hf <- getAppUserDataDirectory "silly-joy.history"
    return $ defaultSettings { historyFile = Just hf }
