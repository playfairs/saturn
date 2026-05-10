module Main where

import Mycfg.CLI.Parser (parseOptions)
import Mycfg.Core.Engine (runEngine)
import Mycfg.Logging.Logger (initLogger)

main :: IO ()
main = do
  logger <- initLogger
  options <- parseOptions
  runEngine logger options
