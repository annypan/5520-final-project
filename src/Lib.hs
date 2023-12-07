module Lib where
import Syntax
import PrettyPrinter
import FlowParser
import TypeChecker
import Control.Monad (unless)

prompt :: IO ()
prompt = do
    putStrLn "Enter a filename or ':q' to quit"
    l <- getLine
    unless (l == ":q") $ do
        res <- checker ("js/" ++ l)
        print (filter (/= Success) (fst res))
        prompt
