module Lib where
import System.IO
import Environment
import Tools
import Data.Maybe
import Control.Monad
import MyMaybe
import Parser
import Interpreter
import Control.Exception
import System.IO.Error
import System.Exit

-- someFunc :: IO ()
-- someFunc = do
--             hSetBuffering stdout NoBuffering
--             putStr "> "
--             -- hFlush stdout
--             line <- getLine
--             putStrLn line
--             someFunc

getFile :: String -> IO ()
getFile file = do
                let test = file
                contents <- readFile test
                putStrLn contents

returnError :: IO a
returnError = exitWith $ ExitFailure 84

readHandler :: IOError -> IO a
readHandler e
    | isDoesNotExistError e = putStrLn "The file does not exist" >> returnError
    | otherwise = putStrLn "Something went wrong" >> returnError

execInstruction :: Env -> String -> Instruction -> IO ()
execInstruction env restCmd inst    | isJust arg && isErrorArgument (fromJust arg) = print (fromErrorArgument (fromJust arg)) >> prompt env restCmd
                                    | isJust arg = print (fromJust arg) >> prompt env' restCmd
                                    | otherwise = prompt env' restCmd
                                    where (env', arg) = execCommand env inst

prompt :: Env -> String -> IO ()
prompt env restCmd = do
                    hSetBuffering stdout NoBuffering
                    putStr "> "
                    -- hFlush stdout
                    line <- getLine
                    let (restCmd', inst) = splitAllInstructions (restCmd ++ line)
                    when (myIsNothing inst && myFromNothing inst /= "") $ printErrorPrompt (myFromNothing inst) env
                    if myIsNothing inst && myFromNothing inst == "" then
                        prompt env restCmd'
                    else
                        execInstruction env restCmd' (myFromJust inst)

myGetFile :: [FilePath] -> String -> IO String
myGetFile [] str = return str
myGetFile ("-i":rest) str = myGetFile rest str
myGetFile (file:rest) str = do
                        -- when (checkExct file) $ exitWith $ ExitFailure 84
                        content <- readFile file `catch` readHandler
                        myGetFile rest (str ++ content)

execFileCommand :: Env -> Instruction -> String -> (Env, Maybe Argument) -> (Env, Maybe Argument)
execFileCommand env inst restCmd last   | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | null restCmd = (env', arg)
                                        | otherwise = execFileLoop env' restCmd (env', arg)
                                        where (env', arg) = execCommand env inst


execFileLoop :: Env -> String -> (Env, Maybe Argument) -> (Env, Maybe Argument)
execFileLoop env [] _ = (env, Nothing)
execFileLoop env cmd last   | myIsNothing inst && myFromNothing inst == "" = last
                            | myIsNothing inst = (env, Just (Instruction (myFromJust inst)))
                            | otherwise = execFileCommand env (myFromJust inst) restCmd last
                            where (restCmd, inst) = splitAllInstructions cmd

printErrorPrompt :: String -> Env -> IO ()
printErrorPrompt str env = putStrLn str >> prompt env []

printError :: String -> IO ()
printError str = putStrLn str >> returnError

wantToLaunchPrompt :: [String] -> Bool
wantToLaunchPrompt [] = False
wantToLaunchPrompt (arg:rest) = arg == "-i" || wantToLaunchPrompt rest

printResult :: Maybe Argument -> IO ()
printResult Nothing = putStr ""
printResult (Just arg)  | isErrorArgument arg = printError (show (fromErrorArgument arg))
                        | otherwise = putStrLn (show arg)

checkExct :: String -> Bool
checkExct [] = False
checkExct ('.':xs) = xs /= "scm"
checkExct (x:xs) = checkExct xs