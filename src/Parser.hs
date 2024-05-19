-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Parser
--

module Parser where

import MyMaybe
import GHC.Show (Show)
import Tools
import Data.Maybe

mixCommandsCarCdr :: String -> String -> MyMaybe Instruction
mixCommandsCarCdr [] _ = MyNothing "Should not happen"
mixCommandsCarCdr ['a'] list = concatInstruction "car" (Command CAR) (splitArguments [list])
mixCommandsCarCdr ['d'] list = concatInstruction  "cdr" (Command CDR) (splitArguments [list])
mixCommandsCarCdr ('a':xs) list | myIsJust inst = concatInstruction "car" (Command CAR) (MyJust [Instruction (myFromJust inst)])
                                | otherwise = inst
                                where inst = mixCommandsCarCdr xs list
mixCommandsCarCdr ('d':xs) list | myIsJust inst = concatInstruction "cdr" (Command CDR) (MyJust [Instruction (myFromJust inst)])
                                | otherwise = inst
                                where inst = mixCommandsCarCdr xs list
mixCommandsCarCdr _ _ = MyNothing "Sould not happen"

concactArgumentsLists :: MyMaybe [Argument] -> MyMaybe [Argument] -> MyMaybe [Argument]
concactArgumentsLists (MyJust args1) (MyJust args2) = MyJust (args1 ++ args2)
concactArgumentsLists (MyNothing err) _ = MyNothing err
concactArgumentsLists _ (MyNothing err) = MyNothing err

parseMixCarCdr :: String -> [String] -> MyMaybe Instruction
parseMixCarCdr [] _ = MyNothing "Should not happen"
parseMixCarCdr ['a'] rest = concatInstruction "car" (Command CAR) (concactArgumentsLists (splitArguments [head rest]) (splitArguments (tail rest)))
parseMixCarCdr ['d'] rest = concatInstruction "cdr" (Command CDR) (concactArgumentsLists (splitArguments [head rest]) (splitArguments (tail rest)))
parseMixCarCdr ('a':xs) rest    | myIsJust inst = concatInstruction "car" (Command CAR) (concactArguments (Instruction (myFromJust inst)) (splitArguments (tail rest)))
                                | otherwise = inst
                                where inst = mixCommandsCarCdr xs (head rest)
parseMixCarCdr ('d':xs) rest    | myIsJust inst = concatInstruction "cdr" (Command CDR) (concactArguments (Instruction (myFromJust inst)) (splitArguments (tail rest)))
                                | otherwise = inst
                                where inst = mixCommandsCarCdr xs (head rest)
parseMixCarCdr _ _ = MyNothing "Should not happen"

checkMixCarCdr :: String -> Bool
checkMixCarCdr [] = False
checkMixCarCdr [x] = x `elem` ['a','d']
checkMixCarCdr (x:xs) = x `elem` ['a','d'] && checkMixCarCdr xs

myIsNumber :: String -> Bool -> Bool
myIsNumber [] _ = True
myIsNumber ('-':x:xs) True = x `elem` ['0'..'9'] && myIsNumber xs False
myIsNumber ('+':x:xs) True = x `elem` ['0'..'9'] && myIsNumber xs False
myIsNumber (x:xs) _ = x `elem` ['0'..'9'] && myIsNumber xs False

getEndOfInstruction :: String -> Int -> (Maybe String, String)
getEndOfInstruction [] _ = (Nothing, [])
getEndOfInstruction (')':xs) 1 = (Just [], xs)
getEndOfInstruction (')':xs) num =  let (str, rest) = getEndOfInstruction xs (num - 1)
                                    in (str >>= (\x -> Just (')' : x)), rest)
getEndOfInstruction ('(':xs) num =  let (str, rest) = getEndOfInstruction xs (num + 1)
                                    in (str >>= (\x -> Just ('(' : x)), rest)
getEndOfInstruction ('\t':xs) num = let (str, rest) = getEndOfInstruction xs num
                                    in (str >>= (\x -> Just (' ' : x)), rest)
getEndOfInstruction ('\n':xs) num = let (str, rest) = getEndOfInstruction xs num
                                    in (str >>= (\x -> Just (' ' : x)), rest)
getEndOfInstruction (x:xs) num =    let (str, rest) = getEndOfInstruction xs num
                                    in (str >>= (\x' -> Just (x : x')), rest)

myWords :: Maybe String -> Maybe [String]
myWords Nothing = Nothing
myWords (Just []) = Just []
myWords (Just (' ':xs)) = myWords $ Just (dropWhile (==' ') xs)
myWords (Just ('(':xs)) | null rest || head rest == ' ' = myWords (Just rest) >>= (\x -> instruction >>= (\y -> Just (('(' : y ++ ")"):x)))
                        | otherwise = Nothing
                        where (instruction, rest) = getEndOfInstruction xs 1
myWords (Just ('\'':'(':xs))    | null rest || head rest == ' ' = myWords (Just rest) >>= (\x -> instruction >>= (\y -> Just (("\'(" ++ y ++ ")"):x)))
                                | otherwise = Nothing
                                where (instruction, rest) = getEndOfInstruction xs 1
myWords (Just all) =    let (first, rest) = span (/=' ') all
                        in myWords (Just rest) >>= (\x -> Just (first : x))

createListArr :: Maybe String -> Maybe [Value]
createListArr Nothing = Nothing
createListArr (Just []) = Just []
createListArr (Just [x])    | myIsNumber [x] False = Just [Integer (read [x] :: Int)]
                            | otherwise = Just [Variable [x]]
createListArr (Just ('(':xs)) = let (instruction, rest) = getEndOfInstruction xs 1
                                in  createListArr instruction >>= (\x -> createListArr (Just rest) >>= (\y -> Just (List x:y)))
createListArr (Just (' ':xs)) = createListArr $ Just (dropWhile (==' ') xs)
createListArr (Just all)    | myIsNumber first True = createListArr (Just rest) >>= (\x -> Just (Integer (read first :: Int):x))
                            | otherwise = createListArr (Just rest) >>= (\x -> Just (Variable first:x))
                            where (first, rest) = span (/=' ') all

concactArguments :: Argument -> MyMaybe [Argument] -> MyMaybe [Argument]
concactArguments arg (MyJust args) = MyJust (arg : args)
concactArguments arg (MyNothing err) = MyNothing err

assembleArguments :: Maybe [Value] -> MyMaybe [Argument] ->  MyMaybe [Argument]
assembleArguments Nothing _ = MyNothing "Expression sould end with ')'"
assembleArguments _ (MyNothing err) = MyNothing err
assembleArguments (Just vals) (MyJust args) = MyJust (Value (List vals):args)

splitArguments :: [String] -> MyMaybe [Argument]
splitArguments [] = MyJust []
splitArguments (('(':xs):rest)  | myIsJust instruction = concactArguments (Instruction (myFromJust instruction)) (splitArguments rest)
                                | otherwise = MyNothing (myFromNothing instruction)
                                where
                                    (instructionStr, rest') = getEndOfInstruction xs 1
                                    wordsInst = myWords instructionStr
                                    instruction = splitInstruction wordsInst
splitArguments (('\'':'(':xs):rest) | null rest' = assembleArguments (createListArr first) (splitArguments rest)
                                    | otherwise = MyNothing "Should be empty"
                                    where (first, rest') = getEndOfInstruction xs 1
splitArguments (('\'':xs):rest) | myIsNumber xs True = concactArguments (Value (Integer (read xs :: Int))) (splitArguments rest)
                                | otherwise = concactArguments (Value (String xs)) (splitArguments rest)
splitArguments (('#':['t']):rest) = concactArguments (Value (Boolean True)) (splitArguments rest)
splitArguments (('#':['f']):rest) = concactArguments (Value (Boolean False)) (splitArguments rest)
splitArguments (x:rest) | isBuiltIn x = concactArguments (Command (textInCommand x)) (splitArguments rest)
                        | myIsNumber x True = concactArguments (Value (Integer (read x :: Int))) (splitArguments rest)
                        | otherwise = concactArguments (Value (Variable x)) (splitArguments rest)

concatInstruction :: String -> Argument -> MyMaybe [Argument] -> MyMaybe Instruction
concatInstruction name arg (MyJust args) = MyJust (Normal (arg : args) name)
concatInstruction name arg (MyNothing err) = MyNothing err

assembleInstruction :: String -> Maybe [Value] -> MyMaybe [Argument] -> MyMaybe Instruction
assembleInstruction _ Nothing _ = MyNothing "Exception: expression should end with ')'"
assembleInstruction _ _ (MyNothing err) = MyNothing err
assembleInstruction name (Just vals) (MyJust args) = MyJust (Normal (Value (List vals):args) name)

splitInstruction :: Maybe [String] -> MyMaybe Instruction
splitInstruction Nothing = MyNothing "Exception: expression should end with ')'"
splitInstruction (Just []) = MyJust (Normal [] "")
splitInstruction (Just ([]:rest)) = MyNothing "Should not be empty"
splitInstruction (Just (('(':xs):rest)) | isNothing wordsInst = MyNothing ""
                                        | myIsJust instruction = concatInstruction (head $ fromJust wordsInst) (Instruction $ myFromJust instruction) (splitArguments rest)
                                        | otherwise = MyNothing (myFromNothing instruction)
                                        where
                                            (instructionStr, rest') = getEndOfInstruction xs 1
                                            wordsInst = myWords instructionStr
                                            instruction = splitInstruction wordsInst
splitInstruction (Just (('\'':'(':xs):rest))   | null rest' = assembleInstruction "list" (createListArr first) (splitArguments rest)
                                        | otherwise = MyNothing "Should be empty"
                                        where (first, rest') = getEndOfInstruction xs 1
splitInstruction (Just (('#':['t']):rest)) = concatInstruction "#t" (Value (Boolean True)) (splitArguments rest)
splitInstruction (Just (('#':['f']):rest)) = concatInstruction "#f" (Value (Boolean False)) (splitArguments rest)
splitInstruction (Just (all@(x:xs):rest))  | isBuiltIn all = concatInstruction all (Command (textInCommand all)) (splitArguments rest)
                                    | x == 'c' && last xs == 'r' && checkMixCarCdr middle = parseMixCarCdr middle rest
                                    | myIsNumber all True = concatInstruction "int" (Value (Integer (read all :: Int))) (splitArguments rest)
                                    | otherwise = concatInstruction all (Value (Variable all)) (splitArguments rest)
                                    where middle = init xs

verifInstruction :: Maybe [Value] -> String -> MyMaybe Instruction
verifInstruction Nothing _ = MyNothing "Arguments should be separated with spaces"
verifInstruction (Just vals) name = MyJust (Normal [Value (List vals)] name)

splitAllInstructions :: String -> (String, MyMaybe Instruction)
splitAllInstructions [] = ([], MyNothing "")
splitAllInstructions ('(':xs) = let (instruction, rest) = getEndOfInstruction xs 1
                                in (rest, splitInstruction (myWords instruction))
splitAllInstructions ('\'':'(':xs) =    let (instruction, rest) = getEndOfInstruction xs 1
                                        in (rest, verifInstruction (createListArr instruction) "list")
splitAllInstructions ('\'':xs)  | myIsNumber quote True = (rest, MyJust (Normal [Value (Integer (read quote :: Int))] "int"))
                                | otherwise = (rest, MyJust (Normal [Value (String quote)] "string"))
                                where (quote, rest) = span (/=' ') xs
splitAllInstructions ('#':'t':xs) = (xs, MyJust (Normal [Value (Boolean True)] "#t"))
splitAllInstructions ('#':'f':xs) = (xs, MyJust (Normal [Value (Boolean False)] "#f"))
splitAllInstructions (' ':xs) = splitAllInstructions (dropWhile (==' ') xs)
splitAllInstructions ('\n':xs) = splitAllInstructions (dropWhile (=='\n') xs)
splitAllInstructions ('\t':xs) = splitAllInstructions (dropWhile (=='\t') xs)
splitAllInstructions all    | isBuiltIn all = (rest, MyJust (Normal [Command (textInCommand all)] all))
                            | myIsNumber str True = (rest, MyJust (Normal [Value (Integer (read str :: Int))] "int"))
                            | otherwise = (rest, MyJust (Normal [Value (Variable str)] str))
                            where (str, rest) = span (/=' ') all