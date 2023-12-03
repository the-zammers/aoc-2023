module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import System.Environment (getArgs)
import Data.Char (isDigit)

main :: IO ()
main = do
  args <- getArgs
  case map (filter isDigit) args of
    "01" : _ -> day01
    "02" : _ -> day02
    "03" : _ -> day03
    _ -> error "None or invalid day number provided."
