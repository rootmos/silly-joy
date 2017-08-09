{-# LANGUAGE ApplicativeDo #-}
module Main where

import UI (runRepl, runTui)

import Options.Applicative
import Data.Semigroup ((<>))

data Config = MkConfig { tui :: Bool }

config :: Parser Config
config = do
    s <- switch ( long "tui" <> short 't' <> help "Run with terminal UI")
    return $ MkConfig s

parser :: ParserInfo Config
parser =
    info (config <**> helper)
         (fullDesc <> header "silly-joy - a silly intepreter for joy")

main :: IO ()
main = do
    c <- execParser parser
    case tui c of
      True -> runTui
      False -> runRepl
