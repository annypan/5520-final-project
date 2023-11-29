module Main where

import Lib
import Test.HUnit (runTestTT)
import Test.QuickCheck qualified as QC

main :: IO ()
main = putStrLn someFunc
