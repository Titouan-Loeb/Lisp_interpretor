--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Environment
--

module Environment where

import Data.Maybe
import MyMaybe
import Tools

data Function = Function {
    dname :: String,
    params :: [String],
    instruction :: Instruction
} deriving (Eq)

instance Show Function where
    show fun = "\t\t\t" ++ show (dname fun) ++ "\n\t\t\t" ++ show (params fun) ++ "\n\t\t\t" ++ show (instruction fun)

data GVar = GVar {
    gname :: String,
    gvalue :: Argument
} deriving (Eq)

instance Show GVar where
    show var = show (gname var) ++ ": " ++ show (gvalue var)

data Definition = Var GVar | Func Function
                deriving (Eq)

instance Show Definition where
    show (Var v) = "\t\tVAR:\n" ++ show v ++ "\n"
    show (Func f) = "\t\tFUN:\n" ++ show f ++ "\n"

data Env = Env {
    definitions :: [Definition],
    lvars :: [GVar]
} deriving (Eq)

instance Show Env where
    show env = "Env {\n\tDEF:\n" ++ show (definitions env) ++ "\n\tLET:\n" ++ show (lvars env) ++ "\n}"

isParamAlreadyExisting :: String -> [String] -> Bool
isParamAlreadyExisting name [] = False
isParamAlreadyExisting name (param:rest) = name == param || isParamAlreadyExisting name rest

createParamList :: [Argument] -> MyMaybe [String]
createParamList [] = MyJust []
createParamList ((Value (Variable name)):rest)  | myIsNothing paramList = MyNothing (myFromNothing paramList)
                                                | isParamAlreadyExisting name (myFromJust paramList) = MyNothing "Excepting in define: param is already existing"
                                                | otherwise = MyJust (name : myFromJust paramList)
                                                where paramList = createParamList rest
createParamList _ = MyNothing "Excepting in define: Wrong parameter name"

createVar :: String -> Argument -> GVar
createVar name (Instruction (Normal args _)) = GVar {gname = name, gvalue = Instruction (Normal args name)}
createVar name arg = GVar {gname = name, gvalue = arg}

createDefinition :: String -> [Argument] -> Argument -> MyMaybe Definition
createDefinition name args (Instruction (Normal args' name'))    | myIsNothing newParams = MyNothing (myFromNothing newParams)
                                                        | otherwise = MyJust (Func (Function {  dname = name,
                                                                                                params = myFromJust newParams,
                                                                                                instruction = Normal args' name}))
                                                        where newParams = createParamList args
createDefinition _ _ _ = MyNothing "Exception in define: invalid syntax"

addOrReplaceVar :: [GVar] -> GVar -> [GVar]
addOrReplaceVar [] newVar = [newVar]
addOrReplaceVar (var:rest) newVar   | gname var == gname newVar = newVar : rest
                                    | otherwise = var : addOrReplaceVar rest newVar

concatDefs :: Definition -> MyMaybe [Definition] -> MyMaybe [Definition]
concatDefs def (MyNothing err) = MyNothing err
concatDefs def (MyJust def2) = MyJust (def:def2)

addOrReplaceDef :: [Definition] -> Definition -> MyMaybe [Definition]
addOrReplaceDef [] newDef = MyJust [newDef]
addOrReplaceDef (Var var:rest) (Var newVar) | gname var == gname newVar = MyJust (Var newVar : rest)
addOrReplaceDef (Var var:rest) (Func newFunc) | gname var == dname newFunc = MyJust (Func newFunc : rest)
addOrReplaceDef (Func func:rest) (Var newVar) | dname func == gname newVar = MyJust (Var newVar : rest)
addOrReplaceDef (Func func:rest) (Func newFunc) | dname func == dname newFunc = MyJust (Func newFunc : rest)
addOrReplaceDef (def:rest) newDef = concatDefs def (addOrReplaceDef rest newDef)

envAddDef :: Env -> Definition -> MyMaybe Env
envAddDef env def   | myIsJust newDef = MyJust Env {definitions = myFromJust newDef,
                                                    lvars = lvars env}
                    | otherwise = MyNothing (myFromNothing newDef)
                    where newDef = addOrReplaceDef (definitions env) def

concatLetVars :: GVar -> MyMaybe [GVar] -> MyMaybe [GVar]
concatLetVars var vars  | myIsNothing vars = vars
                        | null (myFromJust vars) = MyJust [var]
                        | otherwise = MyJust (var: myFromJust vars)

createLetVars :: [Argument] -> MyMaybe GVar
createLetVars [] = MyNothing "Exception in let: cannot find argument"
createLetVars (Value (Variable name):[Value val]) = MyJust GVar {gname = name, gvalue = Value val}
createLetVars args = MyNothing (show args)

envAddLet :: Env -> [Argument] -> MyMaybe Env
envAddLet env args  | myIsNothing newVars = MyNothing (myFromNothing newVars)
                    | otherwise = MyJust Env {  definitions = definitions env,
                                                lvars = myFromJust newVars : lvars env}
                    where newVars = createLetVars args