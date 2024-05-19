--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Tools
--

module Tools where
import Prelude
data Value = List [Value]
            | Variable String
            | String String
            | Integer Int
            | Boolean Bool
            | Pair Value Value
            deriving (Eq)

instance Show Value where
    show (Variable str) = str
    show (Integer nb) = show nb
    show (Pair val1 val2) = "(" ++ show val1 ++ " . " ++ show val2 ++ ")"
    show (Boolean b)    | b = "#t"
                        | otherwise = "#f"
    show (String str) = str
    show (List list) = "(" ++ showVals list ++ ")"

                            -- Say if it's done
data Command = QUOTE        -- > Done
                | COND      -- > Done
                | LAMBDA    -- >
                | DEFINE    -- > Done
                | LET       -- > Done
                | CONS      -- > Done
                | CAR       -- > Done
                | CDR       -- > Done
                | ATOM      -- > Done
                | EQUAL     -- > Done
                | LIST      -- (Bonus) > Done
                | INF       -- > Done
                | ADD       -- > Done
                | SUB       -- > Done
                | MUL       -- > Done
                | DIV       -- > Done
                | MOD       -- > Done
                | SQRT      -- > Done
                | IF        -- (Bonus) > Done
                deriving (Show, Eq)

data Argument = Instruction Instruction
                | Command Command
                | Value Value
                | ErrorArgument String
                deriving (Eq)

instance Show Argument where
    show (Command cmd) = commandInText cmd
    show (Value val) = show val
    show (ErrorArgument err) = show err
    show (Instruction inst) = show inst

data Instruction = Normal [Argument] String
                    deriving (Eq)

instance Show Instruction where
    show (Normal [] _) = ""
    show (Normal args name) | name == "" = "#<procedure>"
                            | otherwise = "#<procedure " ++ name ++ ">"

showVals :: [Value] -> String
showVals [] = []
showVals [val] = show val
showVals (val:rest) = show val ++ " " ++ showVals rest

showArgs :: [Argument] -> String
showArgs [] = []
showArgs [val] = show val
showArgs (val:rest) = show val ++ " " ++ showArgs rest

commandInText :: Command -> String
commandInText QUOTE = "quote"
commandInText COND = "cond"
commandInText LAMBDA = "lambda"
commandInText DEFINE = "define"
commandInText LET = "let"
commandInText CONS = "cons"
commandInText CAR = "car"
commandInText CDR = "cdr"
commandInText ATOM = "atom?"
commandInText EQUAL = "eq?"
commandInText LIST = "list?"
commandInText INF = "<"
commandInText ADD = "+"
commandInText SUB = "-"
commandInText MUL = "*"
commandInText DIV = "div"
commandInText MOD = "mod"
commandInText SQRT = "sqrt"
commandInText IF = "if"

textInCommand :: String -> Command
textInCommand "quote" = QUOTE
textInCommand "cond" = COND
textInCommand  "lambda" = LAMBDA
textInCommand "define" = DEFINE
textInCommand "let" = LET
textInCommand "cons" = CONS
textInCommand "car" = CAR
textInCommand "cdr" = CDR
textInCommand "atom?" = ATOM
textInCommand "eq?" = EQUAL
textInCommand "list?" = LIST
textInCommand "<" = INF
textInCommand "+" = ADD
textInCommand "-" = SUB
textInCommand "*" = MUL
textInCommand "div" = DIV
textInCommand "mod" = MOD
textInCommand "sqrt" = SQRT
textInCommand "if" = IF
textInCommand str = error ("Error: " ++ str ++ " is not a built-in command")

isBuiltIn :: String -> Bool
isBuiltIn "quote" = True
isBuiltIn "cond" = True
isBuiltIn "lambda" = True
isBuiltIn "define" = True
isBuiltIn "if" = True
isBuiltIn "let" = True
isBuiltIn "car" = True
isBuiltIn "cdr" = True
isBuiltIn "cons" = True
isBuiltIn "atom?" = True
isBuiltIn "eq?" = True
isBuiltIn "list?" = True
isBuiltIn "<" = True
isBuiltIn "+" = True
isBuiltIn "-" = True
isBuiltIn "*" = True
isBuiltIn "div" = True
isBuiltIn "mod" = True
isBuiltIn "sqrt" = True
isBuiltIn _ = False

isBuiltInC :: Command -> Bool
isBuiltInC QUOTE = True
isBuiltInC COND = True
isBuiltInC LAMBDA = True
isBuiltInC DEFINE = True
isBuiltInC LET = True
isBuiltInC CONS = True
isBuiltInC CAR = True
isBuiltInC CDR = True
isBuiltInC ATOM = True
isBuiltInC EQUAL = True
isBuiltInC LIST = True
isBuiltInC INF = True
isBuiltInC ADD = True
isBuiltInC SUB = True
isBuiltInC MUL = True
isBuiltInC DIV = True
isBuiltInC MOD = True
isBuiltInC SQRT = True
isBuiltInC IF = True
