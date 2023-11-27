module Main where

import Lib
import Test.HUnit (runTestTT)
import Test.QuickCheck qualified as QC
import TypeChecker (testDoesValueMatchPrimitiveType)

main :: IO ()
main = putStrLn someFunc
