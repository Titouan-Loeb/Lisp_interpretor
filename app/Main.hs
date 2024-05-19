module Main where

import Lib
import Control.Monad
import Environment
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        let env = Env {definitions = [], lvars = []}
        contents <- myGetFile args []
        let (env', arg) = execFileLoop env contents (env, Nothing)
        printResult arg
        when (null args || wantToLaunchPrompt args) $
            prompt env' []
