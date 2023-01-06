module CpDump where


import CpAdt

import Data.List (intercalate, intersperse)
import CpParser (identifier)
import MyParser (Parser(run))

class DumpPython program where
    dump :: (State, program) -> (State, String)
    
class DumpPythonStateless expr where
    dumpE :: expr -> String

indent :: Int -> String
indent n = replicate (4*n) ' '

instance DumpPython CpFlow where
    dump :: (State, CpFlow) -> (State, String)
    dump (state, CpFlow []) = (state, "")

    dump (state, CpFlow (head@(CpFlow _) : tail)) =
        (next2state, output ++ next2output)
        where
            (nextState, output) = dump (state, head)
            (next2state, next2output) = dump (nextState, CpFlow tail)

    -- Python cannot declare array. Array must be initialised before access.
    dump (  state@(State indentation),
            CpFlow (
                (CpSingleStatement
                    (CpDeclare
                        variable
                        arrayType@(CpTypeArray dimensions elementType)
                    )
                ):tail)) =
        (   state,
            indent indentation ++ declarationOutput ++ "\n" ++
            indent indentation ++ initialisationOutput ++ "\n" ++
            nextOutput)
        where
            declarationOutput = dumpE variable ++ ": " ++ dumpE arrayType
            -- Objects in the array should be initialised
            -- according to their data type.
            initialiseObject = case elementType of {
                CpTypeInteger -> "0";
                CpTypeReal -> "0.0";
                CpTypeBoolean -> "False";
                CpTypeChar -> "''";
                CpTypeString -> "''";
                CpTypeCustom (CpVariable customType) -> customType ++ "()";
                _ -> "...";
            }
            initialisationOutput =
                dumpE variable ++
                " = " ++
                concat (replicate (length dimensions) "[") ++
                
                if length dimensions == 1 then
                    let (from, to) = head dimensions in
                    initialiseObject ++ " for _ in range(" ++
                    dumpE from ++
                    ", " ++
                    dumpE to ++ "+1)]"
                else
                    -- If this is an multi-dimensional array,
                    -- add a line break for each dimension
                    initialiseObject ++ "\n" ++
                    intercalate "\n" (
                        (\(from, to) ->
                            indent (indentation+1) ++
                            "for _ in range(" ++
                                dumpE from ++
                                ", " ++
                                dumpE to ++ "+1)]")
                        <$> reverse dimensions)

            (_, nextOutput) = dump (state, CpFlow tail)

    dump (  state@(State indentation),
            CpFlow (
                (CpSingleStatement
                    (CpDeclare
                        variable
                        arrayType@(CpTypeCustom customTypeName)
                    )
                ):tail)) =
        (   state,
            indent indentation ++ declarationOutput ++ "\n" ++
            indent indentation ++ initialisationOutput ++ "\n" ++
            nextOutput)
        where
            declarationOutput = dumpE variable ++ ": " ++ dumpE arrayType
            initialisationOutput =
                dumpE variable ++ " = " ++ dumpE customTypeName ++ "()"

            (_, nextOutput) = dump (state, CpFlow tail)
    
    dump (state@(State indentation),
          CpFlow ((
            CpSingleStatement(
                CpEnumerated variable constants)):tail)) =
        (state, output ++ nextOutput)
        where
            output =
                indent indentation ++
                "# " ++ dumpE variable ++ " - Enumerated\n" ++
                indent indentation ++
                "class " ++ dumpE variable ++ ": pass # Placeholder\n" ++

                concatMap (\constant ->
                    indent indentation ++
                    "class " ++ dumpE constant ++ ": pass\n")
                constants

            (_, nextOutput) = dump (state, CpFlow tail)

    dump (state@(State indentation),
          CpFlow ((CpSingleStatement statement):tail)) =
        (state, indent indentation ++ output ++ "\n" ++ nextOutput)
        where
            output = dumpE statement
            (_, nextOutput) = dump (state, CpFlow tail)
    
    dump (  state@(State indentation),
            CpFlow ((CpIf condition thenClause):tail)) =
        (state, output)
        where
            (_, thenClauseOutput) =
                dump (State (indentation+1), thenClause)
            (_, afterEndIfOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "if " ++ dumpE condition ++ ":\n" ++
                    thenClauseOutput ++
                afterEndIfOutput

    dump (  state@(State indentation),
            CpFlow ((CpIfElse condition thenClause elseClause):tail)) =
        (state, output)
        where
            (_, thenClauseOutput) =
                dump (State (indentation+1), thenClause)
            (_, elseClauseOutput) = 
                dump (State (indentation+1), elseClause)
            (_, afterEndIfOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "if " ++ dumpE condition ++ ":\n" ++
                    thenClauseOutput ++
                indent indentation ++ "else:\n" ++
                    elseClauseOutput ++
                afterEndIfOutput

    dump (  state@(State indentation),
            CpFlow ((CpWhile condition loopClause):tail)) =
        (state, output)
        where
            (_, loopClauseOutput) =
                dump (State (indentation+1), loopClause)
            (_, afterEndwhileOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "while " ++ dumpE condition ++ ":\n" ++
                    loopClauseOutput ++
                afterEndwhileOutput

    dump (  state@(State indentation),
            CpFlow ((CpRepeat loopClause condition):tail)) =
        (state, output)
        where
            (_, loopClauseOutput) = 
                dump (State (indentation+1), loopClause)
            (_, afterUntilOutput) = dump (state, CpFlow tail)
            
            output = 
                indent indentation ++ "while True:\n" ++
                    loopClauseOutput ++
                indent (indentation + 1) ++ "if " ++ dumpE condition ++ ": break\n" ++
                afterUntilOutput

    dump (  state@(State indentation),
            CpFlow ((CpFor variable from to loopClause):tail)) =
        (state, output)
        where
            (_, loopClauseOutput) =
                dump (State (indentation+1), loopClause)
            (_, afterNextOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "for " ++ dumpE variable ++ " in range(" ++
                dumpE from ++ ", " ++ dumpE to ++ "+1):\n" ++
                    loopClauseOutput ++
                afterNextOutput
    
    dump (  state@(State indentation),
            CpFlow ((CpForStep variable from to step loopClause):tail)) =
        (state, output)
        where
            (_, loopClauseOutput) =
                dump (State (indentation+1), loopClause)
            (_, afterNextOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "for " ++ dumpE variable ++ " in range(" ++
                dumpE from ++ ", " ++ dumpE to ++ "+1, " ++ dumpE step ++ "):\n" ++
                    loopClauseOutput ++
                afterNextOutput

    dump(   state@(State indentation),
            CpFlow ((CpDefineStruct name declareClauses):tail)) = 
        (state, output)
        where
            (_, declareClausesOutput) =
                dump (State (indentation+1), declareClauses)
            (_, afterEndtypeOutput) = dump (state, CpFlow tail)

            output = 
                indent indentation ++ "class " ++ dumpE name ++ ":\n" ++
                declareClausesOutput ++ afterEndtypeOutput

    dump(   state@(State indentation),
            CpFlow ((CpDefineProcedure name signature clause):tail)) =
        (state, output)
        where
            (_, clauseOutput) =
                dump (State (indentation+1), clause)
            (_, afterEndprocedureOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "def " ++ dumpE name ++ "(" ++
                intercalate ", " ((\(param, paramType) ->
                    dumpE param ++ ": " ++ dumpE paramType) <$> signature) ++
                ") -> None:\n" ++
                clauseOutput ++ afterEndprocedureOutput
    
    dump(   state@(State indentation),
            CpFlow ((CpDefineFunction name signature returnType clause):tail)) =
        (state, output)
        where
            (_, clauseOutput) =
                dump (State (indentation+1), clause)
            (_, afterEndprocedureOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "def " ++ dumpE name ++ "(" ++
                intercalate ", " ((\(param, paramType) ->
                    dumpE param ++ ": " ++ dumpE paramType) <$> signature) ++
                ") -> " ++ dumpE returnType ++ ":\n" ++
                clauseOutput ++ afterEndprocedureOutput

fileNameToVariableIdentifier :: String -> String
fileNameToVariableIdentifier fileName =
    "file_" ++
    ((\c -> if c `notElem` identifier then '_' else c) <$> fileName)

instance DumpPythonStateless CpStatement where
    dumpE :: CpStatement -> String
    dumpE (CpOutput exprs) =
        "print(" ++ intercalate ", " (dumpE <$> exprs) ++ ")"
    dumpE (CpInput expr) = dumpE expr ++ " = input()"
    dumpE (CpReturn expr) = "return " ++ dumpE expr
    dumpE (CpAssign variable value) = dumpE variable ++ " = " ++ dumpE value
    dumpE (CpFunctionCall expr) = dumpE expr
    dumpE (CpDeclare variable variableType) =
        dumpE variable ++ ": " ++ dumpE variableType

    dumpE (CpOpen (CpString fileName) mode) =
        fileNameToVariableIdentifier fileName ++
        " = open(\"" ++ fileName ++ "\", \"" ++ pyMode ++ "\")"
        where
            pyMode = case mode of {
                FileRead -> "r";
                FileWrite -> "w";
                FileAppend -> "a";
            }

    dumpE (CpClose (CpString fileName)) =
        fileNameToVariableIdentifier fileName ++ ".close()"

    dumpE (CpRead (CpString fileName) variable) =
        dumpE variable ++ " = " ++
        fileNameToVariableIdentifier fileName ++ ".readline()"
    
    dumpE (CpWrite (CpString fileName) expr) =
        fileNameToVariableIdentifier fileName ++ ".write(" ++ dumpE expr ++ ")"

    dumpE CpBlankLine = ""


instance DumpPythonStateless CpExpr where
    dumpE :: CpExpr -> String
    dumpE (CpInt integer) = integer
    dumpE (CpFloat integerPart decimalPart) = integerPart ++ "." ++ decimalPart
    dumpE (CpString string) = "\"" ++ string ++ "\""
    dumpE (CpChar char) = "\"" ++ char:"" ++ "\""
    dumpE CpTrue = "True"
    dumpE CpFalse = "False"
    dumpE (CpVariable variable) = variable
    dumpE (CpFunction functionName exprs) = output where
        params = intercalate ", " $ dumpE <$> exprs
        output = functionName ++ "(" ++ params ++ ")"

    dumpE expr@(CpTakeAttribute x y) =
        encloseBracket expr x (dumpE x) ++ "." ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpIndex x indices) =
        dumpE x ++
        concatMap (\index -> "[" ++ dumpE index ++ "]") indices

    dumpE expr@(CpNot x) = "not " ++ encloseBracket expr x (dumpE x)
    dumpE (CpNegative x) = "-" ++ dumpE x

    dumpE expr@(CpPower x y) =
        encloseBracket expr x (dumpE x) ++ " ** " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpMultiply x y) =
        encloseBracket expr x (dumpE x) ++ " * " ++
        encloseBracket expr y (dumpE y)
    
    dumpE expr@(CpDivide x y@(CpMultiply _ _)) =
        encloseBracket expr x (dumpE x) ++
        " / (" ++ dumpE y ++ ")"

    dumpE expr@(CpDivide x y) =
        encloseBracket expr x (dumpE x) ++ " / " ++
        encloseBracket expr y (dumpE y)
            
    dumpE expr@(CpIntDivide x y@(CpMultiply _ _)) =
        encloseBracket expr x (dumpE x) ++
        " // (" ++ dumpE y ++ ")"

    dumpE expr@(CpIntDivide x y) =
        encloseBracket expr x (dumpE x) ++ " // " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpModulus x y@(CpMultiply _ _)) = 
        encloseBracket expr x (dumpE x) ++
        " % (" ++ dumpE y ++ ")"

    dumpE expr@(CpModulus x y) =
        encloseBracket expr x (dumpE x) ++ " % " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpAdd x y) =
        encloseBracket expr x (dumpE x) ++ " + " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpSubtract x y@(CpAdd _ _)) =
        encloseBracket expr x (dumpE x) ++
        " - (" ++ dumpE y ++ ")"

    dumpE expr@(CpSubtract x y) =
        encloseBracket expr x (dumpE x) ++ " - " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpLess x y) =
        encloseBracket expr x (dumpE x) ++ " < " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpGreater x y) =
        encloseBracket expr x (dumpE x) ++ " > " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpLessEqual x y) =
        encloseBracket expr x (dumpE x) ++ " <= " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpGreaterEqual x y) =
        encloseBracket expr x (dumpE x) ++ " >= " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpEqual x y) =
        encloseBracket expr x (dumpE x) ++ " == " ++
        encloseBracket expr y (dumpE y)
    
    dumpE expr@(CpNotEqual x y) =
        encloseBracket expr x (dumpE x) ++ " != " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpAnd x y) =
        encloseBracket expr x (dumpE x) ++ " and " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpOr x y) =
        encloseBracket expr x (dumpE x) ++ " or " ++
        encloseBracket expr y (dumpE y)

    dumpE expr@(CpBuiltinInt expr1) = "int(" ++ dumpE expr1 ++ ")"

    dumpE expr@(CpBuiltinChr order) = "chr(" ++ dumpE order ++ ")"

    dumpE expr@(CpBuiltinAsc char) = "ord(" ++ dumpE char ++ ")"

    dumpE expr@(CpBuiltinLcase string) = dumpE string ++ ".lower()"

    dumpE expr@(CpBuiltinUcase string) = dumpE string ++ ".upper()"

    dumpE expr@(CpBuiltinToLower string) = dumpE string ++ ".lower()"

    dumpE expr@(CpBuiltinToUpper string) = dumpE string ++ ".upper()"

    dumpE expr@(CpBuiltinLength string) = "len(" ++ dumpE string ++ ")"

    dumpE expr@(CpBuiltinLeft string count) =
        dumpE string ++ "[0:" ++ dumpE count ++ "]"
    
    dumpE expr@(CpBuiltinRight string count) =
        dumpE string ++ "[-" ++ dumpE count ++ ":-1]"

    dumpE expr@(CpBuiltinMid string left count) =
        dumpE string ++ "[" ++
        dumpE left ++ "-1:" ++
        dumpE left ++ "-1+" ++ dumpE count ++ "]"
    
    dumpE expr@(CpBuiltinNumToString number) = "str(" ++ dumpE number ++ ")"

    dumpE expr@(CpBuiltinEof (CpString path)) =
        "EOF(" ++
        fileNameToVariableIdentifier path ++
        ")"
    
    dumpE CpBuiltinRand = "random()"
    

encloseBracket :: Ord a => a -> a -> String -> String
encloseBracket outside inside dumpedInside =
    if outside >= inside
    then dumpedInside
    else "(" ++ dumpedInside ++ ")"

instance DumpPythonStateless CpType where
    dumpE CpTypeString  = "str"
    dumpE CpTypeInteger = "int"
    dumpE CpTypeReal    = "float"
    dumpE CpTypeBoolean = "bool"
    dumpE CpTypeChar    = "str"

    dumpE (CpTypeArray shape elementType) =
        concat (replicate (length shape) "list[") ++
        dumpE elementType ++
        concat (replicate (length shape) "]")
    
    dumpE (CpTypeCustom typeName) = dumpE typeName


newtype State = State Int deriving Show
initialState :: State
initialState = State 0
