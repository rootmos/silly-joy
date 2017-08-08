module Main where

import UI (runRepl, runTui)

import Options.Applicative
import Data.Semigroup ((<>))

data Config = MkConfig { tui :: Bool }

config :: Parser Config
config = MkConfig
    <$> switch ( long "tui"
              <> short 't'
              <> help "Run with terminal UI")

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
