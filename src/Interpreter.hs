--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Interpreter
--

module Interpreter where

import Environment
import Tools
import Data.Maybe
import MyMaybe

isErrorArgument :: Argument -> Bool
isErrorArgument (ErrorArgument str) = True
isErrorArgument _ = False

fromErrorArgument :: Argument -> String
fromErrorArgument (ErrorArgument str) = str
fromErrorArgument _ = error "Error Parser: fromErrorArgument: cannot handle other than ErrorArgument"

execFunction :: Env -> Argument -> Maybe Argument
execFunction env (Instruction inst@(Normal args name)) = snd $ execCommand env inst
execFunction env arg = Just arg

seekForVarDef :: [Definition] -> String -> Env -> Maybe Argument
seekForVarDef [] name _ = Nothing
seekForVarDef (Func fun:rest) name env  | dname fun == name = Just (ErrorArgument ("Exception in " ++ name ++ ": no parameters found"))
                                        | otherwise = seekForVarDef rest name env
seekForVarDef (Var var:rest) name env   | gname var == name = execFunction env (gvalue var)
                                        | otherwise = seekForVarDef rest name env

seekForVarLet :: [GVar] -> String -> Maybe Argument
seekForVarLet [] _ = Nothing
seekForVarLet (var:rest) name   | gname var == name = Just (gvalue var)
                                | otherwise = seekForVarLet rest name

seekForVar :: Env -> String -> Argument
seekForVar env name | isJust letVar = fromJust letVar
                    | isJust defVar = fromJust defVar
                    | otherwise = ErrorArgument ("Exception: variable " ++ name ++ " is not bound")
                    where
                        letVar = seekForVarLet (lvars env) name
                        defVar = seekForVarDef (definitions env) name env

execCommandBI :: Env -> Instruction -> (Env, Maybe Argument)
execCommandBI env (Normal (Command QUOTE:args) _)   | length args /= 1 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (quote)"))
                                                    | otherwise = fquote env args
execCommandBI env (Normal (Command COND:args) _)    | null args = (env, Just (ErrorArgument "Exception: incorrect argument count in call (cond)"))
                                                    | otherwise = fcond env args
-- execCommandBI env (Normal (Command LAMBDA:args)) | length args /= 1 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (lambda)"))
--                                     | otherwise = flambda args
execCommandBI env (Normal (Command DEFINE:args) _)  | length args /= 2 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (define)"))
                                                    | otherwise = fdefine env args
execCommandBI env (Normal (Command IF:args) _)      | length args /= 3 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (if)"))
                                                    | otherwise = fif env args
execCommandBI env (Normal (Command LET:args) _) | length args /= 2 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (let)"))
                                                | otherwise = flet env args
execCommandBI env (Normal (Command CONS:args) _)    | length args /= 2 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (cons)"))
                                                    | otherwise = fcons env args
execCommandBI env (Normal (Command CAR:args) _) | length args /= 1 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (car)"))
                                                | otherwise = fcar env args
execCommandBI env (Normal (Command CDR:args) _) | length args /= 1 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (cdr)"))
                                                | otherwise = fcdr env args
execCommandBI env (Normal (Command ATOM:args) _)    | length args /= 1 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (atom?)"))
                                                    | otherwise = fatom env args
execCommandBI env (Normal (Command EQUAL:args) _)   | length args /= 2 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (eq?)"))
                                                    | otherwise = feq env args
execCommandBI env (Normal (Command LIST:args) _)    | length args /= 1 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (list?)"))
                                                    | otherwise = flist env args
execCommandBI env (Normal (Command INF:args) _) | length args /= 2 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (<)"))
                                                | otherwise = finf env args
execCommandBI env (Normal (Command ADD:args) _) | null args = (env, Just (ErrorArgument "Exception: incorrect argument count in call (+)"))
                                                | otherwise = fadd env args
execCommandBI env (Normal (Command SUB:args) _) | null args = (env, Just (ErrorArgument "Exception: incorrect argument count in call (-)"))
                                                | otherwise = fsub env args
execCommandBI env (Normal (Command MUL:args) _) | null args = (env, Just (ErrorArgument "Exception: incorrect argument count in call (*)"))
                                                | otherwise = fmul env args
execCommandBI env (Normal (Command DIV:args) _) | length args /= 2 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (div)"))
                                                | otherwise = fdiv env args
execCommandBI env (Normal (Command MOD:args) _) | length args /= 2 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (mod)"))
                                                | otherwise = fmod env args
execCommandBI env (Normal (Command SQRT:args) _)    | length args /= 1 = (env, Just (ErrorArgument "Exception: incorrect argument count in call (sqrt)"))
                                                    | otherwise = fsqrt env args
execCommandBI env _ = (env, Just (ErrorArgument "Exception: Command does not exist"))

checkLets :: [GVar] -> String -> Bool
checkLets [] _ = False
checkLets (var:rest) name = (gname var == name) && checkLets rest name

checkDefs :: [Definition] -> String -> Bool
checkDefs [] name = False
checkDefs (Func fun:rest) name  | dname fun == name = True
                                | otherwise = checkDefs rest name
checkDefs (Var var:rest) name   | gname var == name = True
                                | otherwise = checkDefs rest name

isVarExisting :: Env -> String -> Bool
isVarExisting env name =    let varLet = checkLets (lvars env) name
                                varDef = checkDefs (definitions env) name
                            in varLet || varDef

createParams :: Env -> [String] -> [Argument] -> Maybe [GVar]
createParams _ [] [] = Just []
createParams env [] a = Nothing
createParams env a [] = Nothing
createParams env params (ErrorArgument err:restArgs) = Nothing
createParams env params (Instruction inst:restArgs) | isNothing arg = Nothing
                                                    | isErrorArgument (fromJust arg) = Nothing
                                                    | otherwise = createParams env params (fromJust arg:restArgs)
                                                    where (env', arg) = execCommand env inst
createParams env (str:restStr) (Value (Variable var):restArgs) = createParams env (str:restStr) (seekForVar env var : restArgs)
createParams env (str:restStr) (arg:restArgs) = createParams env restStr restArgs >>= (\x -> Just (GVar {gname = str, gvalue = arg}:x))

addParamsToEnv :: Env -> [String] -> [Argument] -> Maybe Env
addParamsToEnv env params args  | isJust newParams =  Just Env {definitions = definitions env,
                                                                lvars = fromJust newParams ++ lvars env}
                                | otherwise = Nothing
                                where newParams = createParams env params args

execDefFunc :: Env -> Instruction -> (Env, Maybe Argument)
execDefFunc env inst =  let (env', arg) = execCommand env inst
                        in (env, arg)

retrieveArgs :: Env -> Function -> [Argument] -> (Env, Maybe Argument)
retrieveArgs env fun args   | isNothing env' = (env, Just (ErrorArgument "Exception: wrong arguments"))
                            | otherwise = (env, snd $ execDefFunc (fromJust env') (instruction fun))
                            where env' = addParamsToEnv env (params fun) args

seekForFunc :: Env -> [Definition] -> String -> [Argument] -> (Env, Maybe Argument)
seekForFunc env [] name _ = (env, Just (ErrorArgument ("Exception in " ++ name ++ ": no parameters found")))
seekForFunc env (Func fun:rest) name args   | dname fun == name = retrieveArgs env fun args
                                            | otherwise = seekForFunc env rest name args
seekForFunc env (Var var:rest) name args    | gname var == name = (env, Just (ErrorArgument "Exception: attempt to apply a non-procedure"))
                                            | otherwise = seekForFunc env rest name args

changeInstName :: Instruction -> String -> Argument
changeInstName (Normal args _) name = Instruction (Normal args name)

returnVarDef :: [Definition] -> String -> Env -> Maybe Argument
returnVarDef [] name _ = Nothing
returnVarDef (Func fun:rest) name env   | dname fun == name = Just (changeInstName (instruction fun) name)
                                        | otherwise = returnVarDef rest name env
returnVarDef (Var var:rest) name env    | gname var == name = execFunction env (gvalue var)
                                        | otherwise = returnVarDef rest name env

returnVarOrFunc :: Env -> String -> Argument
returnVarOrFunc env name    | isJust letVar = fromJust letVar
                            | isJust defVar = fromJust defVar
                            | otherwise = ErrorArgument ("Exception: variable " ++ name ++ " is not bound")
                            where
                                letVar = seekForVarLet (lvars env) name
                                defVar = returnVarDef (definitions env) name env

execCommand :: Env -> Instruction -> (Env, Maybe Argument)
execCommand env (Normal [] _) = (env, Just (ErrorArgument "Exception: invalid syntax"))
execCommand env (Normal [Value (List list)] _) = (env, Just (Value (List list)))
execCommand env (Normal [Value (Boolean bool)] _) = (env, Just (Value (Boolean bool)))
execCommand env (Normal [Value (Integer int)] _) = (env, Just (Value (Integer int)))
execCommand env (Normal [Value (String str)] _) = (env, Just (Value (String str)))
execCommand env (Normal [Value (Variable str)] _) = (env, Just (returnVarOrFunc env str))
execCommand env (Normal [Command cmd] _) = (env, Just (Instruction (Normal [Command cmd] (commandInText cmd))))
execCommand env (Normal (Command command:args) _)   | isVarExisting env cmdTxt = execCommand env (Normal (Value (Variable cmdTxt) : args) cmdTxt)
                                                    | otherwise = execCommandBI env (Normal (Command command:args) cmdTxt)
                                                    where cmdTxt = commandInText command
execCommand env (Normal (Instruction instruction@(Normal _ name):args) _)   | isJust instruction' = execCommand newEnv (Normal (fromJust instruction' : args) name)
                                                                            | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                                                            where (newEnv, instruction') = execCommand env instruction
execCommand env (Normal (Value (Variable name):args) _) =   let (env', arg) = seekForFunc env (definitions env) name args
                                                            in (env, arg)
execCommand env (Normal _ _) = (env, Just (ErrorArgument "Exception: invalid syntax"))

------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

checkCond :: Env -> [Argument] -> Maybe Argument
checkCond env (Instruction inst:[args]) | isNothing arg = Just (ErrorArgument "Exception: invalid syntax")
                                        | isErrorArgument (fromJust arg) = arg
                                        | otherwise = checkCond env (fromJust arg:[args])
                                        where (env', arg) = execCommand env inst
checkCond env (Value (Boolean bool):[rest]) | bool = Just rest
                                            | otherwise = Nothing
checkCond env _ = Just (ErrorArgument "Exception in cond: wrong arguments")

computeCondResult :: Env -> Argument -> (Env, Maybe Argument)
computeCondResult env (Instruction inst) = execCommand env inst
computeCondResult env (Value (Variable var)) = (env, Just (seekForVar env var))
computeCondResult env arg = (env, Just arg)

fcond :: Env -> [Argument] -> (Env, Maybe Argument)
fcond env [] = (env, Nothing)
fcond env (Instruction (Normal args name):rest) | isNothing arg = fcond env rest
                                                | otherwise = computeCondResult env (fromJust arg)
                                                where arg = checkCond env args
fcond env _ = (env, Just (ErrorArgument "Exception in cond: wrong arguments"))

fdefineReturn :: Env -> MyMaybe Definition -> (Env, Maybe Argument)
fdefineReturn env (MyNothing err) = (env, Just (ErrorArgument err))
fdefineReturn env (MyJust (Func fun))   | myIsJust newEnv = (myFromJust newEnv, Nothing)
                                        | otherwise = (env, Just (ErrorArgument (myFromNothing newEnv)))
                                        where newEnv = envAddDef env (Func fun)
fdefineReturn env _ = (env, Just (ErrorArgument "Exception in define: this should not happen"))

fdefine :: Env -> [Argument] -> (Env, Maybe Argument)
fdefine env [] = (env, Just (ErrorArgument "Exception in define: should not happen"))
fdefine env (Value (Variable name):arg:_)   | myIsJust newEnv = (myFromJust newEnv, Nothing)
                                            | otherwise = (env, Just (ErrorArgument (myFromNothing newEnv)))
                                            where newEnv = envAddDef env (Var (createVar name arg))
fdefine env (Instruction (Normal (Value (Variable name):vars) _):arg:_) = fdefineReturn env (createDefinition name vars arg)
fdefine env (Instruction (Normal (Command cmd:vars) _):arg:_) = fdefineReturn env (createDefinition (commandInText cmd) vars arg)
fdefine env _ = (env, Just (ErrorArgument "Exception in define: invalid syntax"))

fquoteCreateList :: [Argument] -> [Value]
fquoteCreateList [] = []
fquoteCreateList [Command arg] = [String (commandInText arg)]
fquoteCreateList [Instruction (Normal args _)] = [List (fquoteCreateList args)]
fquoteCreateList [Value (Variable var)] = [String var]
fquoteCreateList [Value val] = [val]
fquoteCreateList (Command arg:rest) = String (commandInText arg) : fquoteCreateList rest
fquoteCreateList (Instruction (Normal args _):rest) = List (fquoteCreateList args) : fquoteCreateList rest
fquoteCreateList (Value (Variable var):rest) = String var : fquoteCreateList rest
fquoteCreateList (Value val:rest) = val : fquoteCreateList rest
fquoteCreateList _ = error "Impossible happen sometimes"

fquote :: Env -> [Argument] -> (Env, Maybe Argument)
fquote env [Command cmd] = (env, Just (Value (String (commandInText cmd))))
fquote env [Value (Variable var)] = (env, Just (Value (String var)))
fquote env [Value val] = (env, Just (Value val))
fquote env [Instruction (Normal inst _)] = (env, Just (Value (List (fquoteCreateList inst))))
fquote env args = (env, Just (ErrorArgument "Exception in fquote: wrong number of argument"))

execLetBody :: Env -> Argument -> Maybe Argument
execLetBody env (Instruction inst)  | isNothing arg = Just (ErrorArgument "Exception in let: invalid context")
                                    | otherwise = arg
                                    where (_, arg) = execCommand env inst
execLetBody env arg = Just arg

checkInstAsValue :: Env -> Argument -> MyMaybe Env
checkInstAsValue env (Instruction (Normal (a : [Instruction inst]) name))   | isJust arg && isErrorArgument (fromJust arg) = MyNothing (fromErrorArgument (fromJust arg))
                                                                            | isJust arg = checkInstAsValue env (Instruction (Normal (a: [fromJust arg]) name))
                                                                            | otherwise = MyNothing "Exception in let: invalid syntax (4)"
                                                                            where (env', arg) = execCommand env inst
checkInstAsValue env (Instruction (Normal arg name)) = envAddLet env arg
checkInstAsValue _ _ = MyNothing "Exception in let: invalid syntax (3)"

flet :: Env -> [Argument] -> (Env, Maybe Argument)
flet env (Instruction (Normal [] _):[arg]) = (env, execLetBody env arg)
flet env ((Instruction (Normal (var:vars) name)):args)  | myIsNothing env' = (env, Just (ErrorArgument $ myFromNothing env'))
                                                        | otherwise = (env, snd $ flet (myFromJust env') (Instruction (Normal vars name):args)) ----- faire la suite bordel !(ARGS) ~ (args)!
                                                        where
                                                            env' = checkInstAsValue env var
flet env _ = (env, Just (ErrorArgument "Exception in let: invalid syntax (1)"))

fif :: Env -> [Argument] -> (Env, Maybe Argument)
fif env (Instruction instruction:rest) = let (var1, var2) = execCommand env instruction
                                        in case var2 of
                                            Nothing -> (env, Just (ErrorArgument "Exception: invalid context"))
                                            _ -> fif env (fromJust var2:rest)
fif env (Value (Variable name):arg1) = fif env (seekForVar env name:arg1)
fif env (Value (Boolean cond):arg1:arg2:_) = if cond
                                            then (env, Just arg1)
                                            else (env, Just arg2)
fif env _ = (env, Just (ErrorArgument "Exception: cannot handle argument"))

------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

computeIntegers :: Char -> Int -> Argument -> Argument
computeIntegers _ _ (ErrorArgument err) = ErrorArgument err
computeIntegers '+' nb (Value (Integer nb2)) = Value (Integer (nb + nb2))
computeIntegers '-' nb (Value (Integer nb2)) = Value (Integer (nb - nb2))
computeIntegers '*' nb (Value (Integer nb2)) = Value (Integer (nb * nb2))
computeIntegers '/' nb (Value (Integer nb2)) = Value (Integer (div nb nb2))
computeIntegers '%' nb (Value (Integer nb2)) = Value (Integer (mod nb nb2))
computeIntegers _ _ _= ErrorArgument "Error made in computing integers"

fadd2 :: Env -> [Argument] -> Argument
fadd2 env [] = Value (Integer 0)
fadd2 env (Value (Integer nb):args) = computeIntegers '+' nb (fadd2 env args)
fadd2 env (Value (Variable name):args) = fadd2 env (seekForVar env name : args)
fadd2 _ (Value (List list):_) = ErrorArgument ("Exception in +: " ++ show list ++ " is not a number")
fadd2 _ (Value (Pair val1 val2):_) = ErrorArgument ("Exception in +: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")
fadd2 _ (Value (Boolean bool):_) = ErrorArgument ("Exception in +: " ++ str bool ++ " is not a number")
                                    where   str True = "#t"
                                            str False = "#f"
fadd2 env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                            | isJust arg = fadd2 env (fromJust arg:args)
                                            | otherwise = ErrorArgument "Exeption: invalid context"
                                            where (env', arg) = execCommand env instruction
fadd2 env (ErrorArgument str:_) = ErrorArgument str
fadd2 _ _ = ErrorArgument "Exception in +: cannot handle the given parameter"

fadd :: Env -> [Argument] -> (Env, Maybe Argument)
fadd env [] = (env, Just (Value (Integer 0)))
fadd env (Value (Integer nb):args) = (env, Just (computeIntegers '+' nb (fadd2 env args)))
fadd env (Value (Variable name):args) = fadd env (seekForVar env name : args)
fadd env (Value (List list):_) = (env, Just (ErrorArgument ("Exception in +: " ++ show list ++ " is not a number")))
fadd env (Value (Pair val1 val2):_) = (env, Just (ErrorArgument ("Exception in +: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")))
fadd env (Value (Boolean bool):_) = (env, Just (ErrorArgument ("Exception in +: " ++ str bool ++ " is not a number")))
                                    where   str True = "#t"
                                            str False = "#f"
fadd env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = fadd env (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
fadd env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
fadd env _ = (env, Just (ErrorArgument "Exception in +: cannot handle the given parameter"))

fsub2 :: Env -> [Argument] -> Argument
fsub2 _ [] = Value (Integer 0)
fsub2 env (Value (Integer nb):args) = computeIntegers '+' nb (fsub2 env args)
fsub2 env (Value (Variable name):args) = fsub2 env (seekForVar env name:args)
fsub2 _ (Value (List list):_) = ErrorArgument ("Exception in -: " ++ show list ++ " is not a number")
fsub2 _ (Value (Pair val1 val2):_) = ErrorArgument ("Exception in -: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")
fsub2 _ (Value (Boolean bool):_) = ErrorArgument ("Exception in -: " ++ str bool ++ " is not a number")
                                    where   str True = "#t"
                                            str False = "#f"
fsub2 env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                            | isJust arg = fsub2 env (fromJust arg:args)
                                            | otherwise = ErrorArgument "Exeption: invalid context"
                                            where (env', arg) = execCommand env instruction
fsub2 env (ErrorArgument str:_) = ErrorArgument str
fsub2 _ _ = ErrorArgument "Exception in -: cannot handle the given parameter"

fsub :: Env -> [Argument] -> (Env, Maybe Argument)
fsub env [] = (env, Just (ErrorArgument "This should not happen (Exception in -)"))
fsub env [Value (Integer nb)] = (env, Just (Value (Integer (-nb))))
fsub env (Value (Integer nb):args) = (env, Just (computeIntegers '-' nb (fsub2 env args)))
fsub env (Value (Variable name):args) = fsub env (seekForVar env name:args)
fsub env (Value (List list):_) = (env, Just (ErrorArgument ("Exception in -: " ++ show list ++ " is not a number")))
fsub env (Value (Pair val1 val2):_) = (env, Just (ErrorArgument ("Exception in -: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")))
fsub env (Value (Boolean bool):_) = (env, Just (ErrorArgument ("Exception in -: " ++ str bool ++ " is not a number")))
                                    where   str True = "#t"
                                            str False = "#f"
fsub env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = fsub env' (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
fsub env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
fsub env _ = (env, Just (ErrorArgument "Exception in -: cannot handle the given parameter"))

fmul2 :: Env -> [Argument] -> Argument
fmul2 _ [] = Value (Integer 1)
fmul2 env (Value (Integer nb):args) = computeIntegers '*' nb (fmul2 env args)
fmul2 env (Value (Variable name):args) = fmul2 env (seekForVar env name:args)
fmul2 _ (Value (List list):_) = ErrorArgument ("Exception in *: " ++ show list ++ " is not a number")
fmul2 _ (Value (Pair val1 val2):_) = ErrorArgument ("Exception in +: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")
fmul2 _ (Value (Boolean bool):_) = ErrorArgument ("Exception in *: " ++ str bool ++ " is not a number")
                                    where   str True = "#t"
                                            str False = "#f"
fmul2 env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                            | isJust arg = fmul2 env' (fromJust arg:args)
                                            | otherwise = ErrorArgument "Exeption: invalid context"
                                            where (env', arg) = execCommand env instruction
fmul2 env (ErrorArgument str:_) = ErrorArgument str
fmul2 _ _ = ErrorArgument "Exception in *: cannot handle the given parameter"

fmul :: Env -> [Argument] -> (Env, Maybe Argument)
fmul env [] = (env, Just (Value (Integer 1)))
fmul env (Value (Integer nb):args) = (env, Just (computeIntegers '*' nb (fmul2 env args)))
fmul env (Value (Variable name):args) = fmul env (seekForVar env name:args)
fmul env (Value (List list):_) = (env, Just (ErrorArgument ("Exception in *: " ++ show list ++ " is not a number")))
fmul env (Value (Pair val1 val2):_) = (env, Just (ErrorArgument ("Exception in +: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")))
fmul env (Value (Boolean bool):_) = (env, Just (ErrorArgument ("Exception in *: " ++ str bool ++ " is not a number")))
                                    where   str True = "#t"
                                            str False = "#f"
fmul env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = fmul env' (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
fmul env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
fmul env _ = (env, Just (ErrorArgument "Exception in *: cannot handle the given parameter"))

fdiv2 :: Env -> [Argument] -> Argument
fdiv2 _ [] = ErrorArgument "This should not happen (Exception in div2)"
fdiv2 env (Value (Integer 0):_) = ErrorArgument "Exception in div: undefined for 0"
fdiv2 env (Value (Integer nb):_) = Value (Integer nb)
fdiv2 env (Value (Variable name):_) = fdiv2 env [seekForVar env name]
fdiv2 _ (Value (List list):_) = ErrorArgument ("Exception in div: " ++ show list ++ " is not a number")
fdiv2 _ (Value (Pair val1 val2):_) = ErrorArgument ("Exception in div: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")
fdiv2 _ (Value (Boolean bool):_) = ErrorArgument ("Exception in div: " ++ str bool ++ " is not a number")
                                    where   str True = "#t"
                                            str False = "#f"
fdiv2 env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                            | isJust arg = fdiv2 env' (fromJust arg:args)
                                            | otherwise = ErrorArgument "Exeption: invalid context"
                                            where (env', arg) = execCommand env instruction
fdiv2 env (ErrorArgument str:_) = ErrorArgument str
fdiv2 _ _ = ErrorArgument "Exception in div: cannot handle the given parameter"

fdiv :: Env -> [Argument] -> (Env, Maybe Argument)
fdiv env [] = (env, Just (ErrorArgument "This should not happen (Exception in div)"))
fdiv env (Value (Integer nb):args) = (env, Just (computeIntegers '/' nb (fdiv2 env args)))
fdiv env (Value (Variable name):args) = fdiv env (seekForVar env name:args)
fdiv env (Value (List list):_) = (env, Just (ErrorArgument ("Exception in div: " ++ show list ++ " is not a number")))
fdiv env (Value (Pair val1 val2):_) = (env, Just (ErrorArgument ("Exception in div: (" ++ show val1 ++ " . " ++ show val2 ++ ") is not a number")))
fdiv env (Value (Boolean bool):_) = (env, Just (ErrorArgument ("Exception in div: " ++ str bool ++ " is not a number")))
                                    where   str True = "#t"
                                            str False = "#f"
fdiv env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = fdiv env' (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
fdiv env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
fdiv env _ = (env, Just (ErrorArgument "Exception in div: cannot handle the given parameter"))

fmod2 :: Env -> [Argument] -> Argument
fmod2 _ [] = ErrorArgument "This should not happen (Exception in mod2)"
fmod2 env (Value (Integer 0):_) = ErrorArgument "Exception in mod: undefined for 0"
fmod2 env (Value (Integer nb):_) = Value (Integer nb)
fmod2 env (Value (Variable name):_) = fmod2 env [seekForVar env name]
fmod2 _ (Value (List list):_) = ErrorArgument ("Exception in mod: " ++ show list ++ " is not a number")
fmod2 _ (Value (Boolean bool):_) = ErrorArgument ("Exception in mod: " ++ str bool ++ " is not a number")
                                    where   str True = "#t"
                                            str False = "#f"
fmod2 env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                            | isJust arg = fmod2 env' (fromJust arg:args)
                                            | otherwise = ErrorArgument "Exeption: invalid context"
                                            where (env', arg) = execCommand env instruction
fmod2 env (ErrorArgument str:_) = ErrorArgument str
fmod2 _ _ = ErrorArgument "Exception in mod: cannot handle the given parameter"

fmod :: Env -> [Argument] -> (Env, Maybe Argument)
fmod env [] = (env, Just (ErrorArgument "This should not happen (Exception in mod)"))
fmod env (Value (Integer nb):args) = (env, Just (computeIntegers '%' nb (fmod2 env args)))
fmod env (Value (Variable name):args) = fmod env (seekForVar env name:args)
fmod env (Value (List list):_) = (env, Just (ErrorArgument ("Exception in mod: " ++ show list ++ " is not a number")))
fmod env (Value (Boolean bool):_) = (env, Just (ErrorArgument ("Exception in mod: " ++ str bool ++ " is not a number")))
                                    where   str True = "#t"
                                            str False = "#f"
fmod env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = fmod env' (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
fmod env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
fmod env _ = (env, Just (ErrorArgument "Exception in mod: cannot handle the given parameter"))

fsqrt :: Env -> [Argument] -> (Env, Maybe Argument)
fsqrt env [] = (env, Just (ErrorArgument "This should not happen (Exception in mod)"))
fsqrt env (Value (Integer nb):args) | nb < 0 = (env, Just (ErrorArgument "Exception in sqrt: cannot handle negative numbers"))
                                    | otherwise = (env, Just (Value (Integer (truncate (sqrt (fromIntegral nb))))))
fsqrt env (Value (Variable name):args) = fsqrt env (seekForVar env name:args)
fsqrt env (Value (List list):_) = (env, Just (ErrorArgument ("Exception in sqrt: " ++ show list ++ " is not a number")))
fsqrt env (Value (Boolean bool):_) = (env, Just (ErrorArgument ("Exception in sqrt: " ++ str bool ++ " is not a number")))
                                    where   str True = "#t"
                                            str False = "#f"
fsqrt env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                            | isJust arg = fsqrt env' (fromJust arg:args)
                                            | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                            where (env', arg) = execCommand env instruction
fsqrt env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
fsqrt env _ = (env, Just (ErrorArgument "Exception in sqrt: cannot handle the given parameter"))

------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

compareArguments :: Argument -> Argument -> Argument
compareArguments arg1 arg2 = Value (Boolean (arg1 == arg2))

feq2 :: Env -> [Argument] -> Argument
feq2 env (Value (Variable name):args) = feq2 env (seekForVar env name:args)
feq2 _ (Value val:_) = Value val
feq2 env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                        | isJust arg = feq2 env' (fromJust arg:args)
                                        | otherwise = ErrorArgument "Exeption: invalid context"
                                        where (env', arg) = execCommand env instruction
feq2 _ _ = ErrorArgument "Exception in eq?: cannot handle the given parameter"

feq :: Env -> [Argument] -> (Env, Maybe Argument)
feq env (Value (Variable name):args) = feq env (seekForVar env name:args)
feq env (Value val:args) = (env, Just (compareArguments (Value val) (feq2 env args)))
feq env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
feq env (Instruction instruction:args)  | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = feq env' (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
feq env _ = (env, Just (ErrorArgument "Exception in eq?: cannot handle the given parameter"))

fatom :: Env -> [Argument] -> (Env, Maybe Argument)
fatom env [Value (List list)] = (env, Just (Value (Boolean (null list))))
fatom env [Value (Pair _ _)] = (env, Just (Value (Boolean False)))
fatom env [Value (Variable name)] = fatom env [seekForVar env name]
fatom env [Value val] = (env, Just (Value (Boolean True)))
fatom env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
fatom env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                            | isJust arg = fatom env' (fromJust arg:args)
                                            | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                            where (env', arg) = execCommand env instruction
fatom env _ = (env, Just (ErrorArgument "Exception in eq?: cannot handle the given parameter"))

compareIntAndArg :: Int -> Argument -> Argument
compareIntAndArg nb (Value (Integer nb2)) = Value (Boolean (nb < nb2))
compareIntAndArg _ (Value val) = ErrorArgument ("Exception in <: " ++ show val ++ " is not a real number")
compareIntAndArg _ _ = ErrorArgument "Exception in <: cannot handle the given parameter"

finf2 :: Env -> [Argument] -> Argument
finf2 _ (Value (Integer nb):_) = Value (Integer nb)
finf2 env (Value (Variable name):_) = finf2 env [seekForVar env name]
finf2 env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                            | isJust arg = finf2 env' (fromJust arg:args)
                                            | otherwise = ErrorArgument "Exeption: invalid context"
                                            where (env', arg) = execCommand env instruction
finf2 _ (Value val:_) = ErrorArgument ("Exception in <: " ++ show val ++ " is not a real number")
finf2 _ _ = ErrorArgument "Exception in <: cannot handle the given parameter"

finf :: Env -> [Argument] -> (Env, Maybe Argument)
finf env (Value (Integer nb):args) = (env, Just (compareIntAndArg nb (finf2 env args)))
finf env (Value (Variable name):args) = finf env (seekForVar env name:args)
finf env (Value val:_) = (env, Just (ErrorArgument ("Exception in <: " ++ show val ++ " is not a real number")))
finf env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
finf env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = finf env' (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
finf env _ = (env, Just (ErrorArgument "Exception in eq?: cannot handle the given parameter"))

flist :: Env -> [Argument] -> (Env, Maybe Argument)
flist env (Value (List list):_) = (env, Just (Value (Boolean True)))
flist env (Value (Variable name):_) = flist env [seekForVar env name]
flist env (Value _:_) = (env, Just (Value (Boolean False)))
flist env (ErrorArgument str:_) = (env, Just (ErrorArgument str))
flist env (Instruction instruction:args)    | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                            | isJust arg = flist env (fromJust arg:args)
                                            | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                            where (env', arg) = execCommand env instruction
flist env _ = (env, Just (ErrorArgument "Exception in eq?: cannot handle the given parameter"))

------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

fcar :: Env -> [Argument] -> (Env, Maybe Argument)
fcar env [] = (env, Just (ErrorArgument "Exception in car: no parameter found"))
fcar env (Value (Pair val1 val2):_) = (env, Just (Value val1))
fcar env (Value (List (val:_)):_) = (env, Just (Value val))
fcar env (Value (List []):_) = (env, Just (ErrorArgument "Exception in car: () is not a pair"))
fcar env (Value (Variable name):_) = fcar env [seekForVar env name]
fcar env (Value (Integer int):_) = (env, Just (ErrorArgument ("Exception in car: " ++ show int ++ " is not a pair")))
fcar env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = fcar env (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
fcar env (param:_) = (env, Just (ErrorArgument ("Exception in car: cannot handle the given parameter " ++ show param)))

fcdr :: Env -> [Argument] -> (Env, Maybe Argument)
fcdr env (Value (Pair val1 val2):_) = (env, Just (Value val2))
fcdr env (Value (List (_:rest)):_) = (env, Just (Value (List rest)))
fcdr env (Value (List []):_) = (env, Just (ErrorArgument "Exception in cdr: () is an empty list"))
fcdr env (Value (Variable name):_) = fcdr env [seekForVar env name]
fcdr env (Value (Integer int):_) = (env, Just (ErrorArgument ("Exception in cdr: " ++ show int ++ " is not a pair")))
fcdr env (Instruction instruction:args) | isJust arg && isErrorArgument (fromJust arg) = (env, arg)
                                        | isJust arg = fcdr env (fromJust arg:args)
                                        | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                        where (env', arg) = execCommand env instruction
fcdr env _ = (env, Just (ErrorArgument "Exception in cdr: cannot handle the given parameter"))

consConcat :: Argument -> Argument -> Argument
consConcat _ (ErrorArgument str) = ErrorArgument str
consConcat (ErrorArgument str) _ = ErrorArgument str
consConcat (Value val) (Value (List list)) = Value (List (val:list))
consConcat (Value val) (Value (Pair left right)) = Value (Pair (List (val:[left])) right)
consConcat (Value val) (Value val2) = Value (Pair val val2)
consConcat _ _ = ErrorArgument "Exception in cons: cannot handle the given parameter"

fcons2 :: Env -> Argument -> Argument
fcons2 env (Value (Variable name)) = fcons2 env (seekForVar env name)
fcons2 env (Value val) = Value val
fcons2 env (Instruction instruction)    | isJust arg && isErrorArgument (fromJust arg) = fromJust arg
                                        | isJust arg = fcons2 env (fromJust arg)
                                        | otherwise = ErrorArgument "Exeption: invalid context"
                                        where (env', arg) = execCommand env instruction
fcons2 _ arg = ErrorArgument $ "Exception in cons: cannot handle the given parameter (2)" ++ show arg

fcons :: Env -> [Argument] -> (Env, Maybe Argument)
fcons env (arg:Value val2@(List list):_) = (env, Just (consConcat (fcons2 env arg) (Value (List list))))
fcons env (arg:Value val2@(Pair left right):_) = (env, Just (consConcat (fcons2 env arg) (Value (Pair left right))))
fcons env (arg:Value (Variable name):_) = fcons env (arg:[seekForVar env name])
fcons env (arg:Value val2:_) = (env, Just (consConcat (fcons2 env arg) (Value val2)))
fcons env (arg:Instruction instruction:_)   | isJust arg' && isErrorArgument (fromJust arg') = (env, arg')
                                            | isJust arg' = fcons env (arg:[fromJust arg'])
                                            | otherwise = (env, Just (ErrorArgument "Exeption: invalid context"))
                                            where (env', arg') = execCommand env instruction
fcons env arg = (env, Just (ErrorArgument $ "Exception in cons: cannot handle the given parameter (1)" ++ show arg))

------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------