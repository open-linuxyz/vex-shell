module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-n", envName] -> runCommand $ New envName
    ["--new", envName] -> runCommand $ New envName
    ["-z"] -> runCommand Reset
    ["--help"] -> runCommand Help
    ["-h"] -> runCommand Help
    [envName] -> runCommand $ Load envName
    _ -> runCommand Choose
