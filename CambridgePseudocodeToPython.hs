{-# LANGUAGE LambdaCase #-}
module CambridgePseudocodeToPython (translate) where

import MyParser
import Control.Applicative (optional, many, some, (<|>), Alternative (empty))
import Data.List (intercalate, intersperse)
import Data.Maybe (fromJust, isJust)
import Data.Char (toLower)


alphaUpper :: String
alphaUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
alphaLower :: String
alphaLower = "abcdefghijklmnopqrstuvwxyz"
alpha :: String
alpha = alphaUpper ++ alphaLower
numeric :: String
numeric = "0123456789"
numericNo0 :: String
numericNo0 = "123456789"
identifier = alpha ++ numeric ++ "_"

-- Priorities of operations ----------------------------------------------------
class HasPriority a where
    priority :: a -> Integer

instance Eq CpExpr where
    (==) :: CpExpr -> CpExpr -> Bool
    a == b = priority a == priority b

instance Ord CpExpr where
    (<=) :: CpExpr -> CpExpr -> Bool
    a <= b = priority a <= priority b

instance HasPriority CpExpr where
    priority :: CpExpr -> Integer
    priority (CpIndex{}) = 1
    priority (CpNot{}) = 2
    priority (CpNegative{}) = 2
    priority (CpPower{}) = 3
    priority (CpMultiply{}) = 4
    priority (CpDivide{}) = 4
    priority (CpModulus{}) = 4
    priority (CpIntDivide{}) = 4
    priority (CpAdd{}) = 5
    priority (CpSubtract{}) = 5
    priority (CpLess{}) = 6
    priority (CpGreater{}) = 6
    priority (CpLessEqual{}) = 6
    priority (CpGreaterEqual{}) = 6
    priority (CpEqual{}) = 7
    priority (CpNotEqual{}) = 7
    priority (CpAnd{}) = 8
    priority (CpOr{}) = 9
    priority _ = 0


-- ADT for Cambridge Pseudocode ------------------------------------------------
data CpExpr
    = CpInt String
    | CpFloat String String
    | CpString String
    | CpChar Char
    | CpTrue
    | CpFalse
    | CpVariable String
    | CpFunction String [CpExpr]

    | CpAdd CpExpr CpExpr       -- +
    | CpSubtract CpExpr CpExpr  -- -
    | CpMultiply CpExpr CpExpr  -- *
    | CpDivide CpExpr CpExpr    -- /
    | CpIntDivide CpExpr CpExpr -- DIV
    | CpModulus CpExpr CpExpr   -- MOD
    | CpPower CpExpr CpExpr     -- ^

    | CpNot CpExpr              -- NOT
    | CpAnd CpExpr CpExpr       -- AND
    | CpOr CpExpr CpExpr        -- OR

    | CpNegative CpExpr         -- -

    | CpLess CpExpr CpExpr          -- <
    | CpGreater CpExpr CpExpr       -- >
    | CpLessEqual CpExpr CpExpr     -- <=
    | CpGreaterEqual CpExpr CpExpr  -- >=
    | CpEqual CpExpr CpExpr         -- =
    | CpNotEqual CpExpr CpExpr      -- <>
    
    | CpIndex CpExpr [CpExpr]

    -- Built-int functions
    | CpBuiltinLength CpExpr
    | CpBuiltinLeft CpExpr CpExpr
    | CpBuiltinRight CpExpr CpExpr
    | CpBuiltinMid CpExpr CpExpr CpExpr
    | CpBuiltinNumToString CpExpr
    deriving Show

data CpType
    = CpTypeInteger
    | CpTypeReal
    | CpTypeBoolean
    | CpTypeString
    | CpTypeChar
    | CpTypeArray [(CpExpr, CpExpr)] CpType
    | CpTypeCustom CpExpr
    deriving Show

data FileMode
    = FileRead
    | FileWrite
    | FileAppend
    deriving Show

data CpStatement
    = CpAssign CpExpr CpExpr
    | CpDeclare CpExpr CpType
    | CpInput CpExpr
    | CpOutput [CpExpr]
    | CpEnumerated CpExpr [CpExpr]
    | CpFunctionCall CpExpr
    | CpReturn CpExpr
    | CpOpen CpExpr FileMode
    | CpWrite CpExpr CpExpr
    | CpRead CpExpr CpExpr
    | CpClose CpExpr
    | CpBlankLine
    deriving Show

data CpFlow
    = CpFlow [CpFlow]
    | CpSingleStatement CpStatement
    | CpIf CpExpr CpFlow
    | CpIfElse CpExpr CpFlow CpFlow
    | CpWhile CpExpr CpFlow
    | CpRepeat CpFlow CpExpr
    | CpFor CpExpr CpExpr CpExpr CpFlow
    | CpForStep CpExpr CpExpr CpExpr CpExpr CpFlow
    | CpDefineStruct CpExpr CpFlow
    | CpDefineProcedure CpExpr [(CpExpr, CpType)] CpFlow
    | CpDefineFunction CpExpr [(CpExpr, CpType)] CpType CpFlow
    deriving Show


-- Parser for Cambridge Pseudocode ---------------------------------------------
-- Expression parser
cpTrueP :: Parser CpExpr
cpTrueP = CpTrue <$ strP "TRUE"

cpFalseP :: Parser CpExpr
cpFalseP = CpFalse <$ strP "FALSE"

cpCharP :: Parser CpExpr
cpCharP = CpChar <$> (charP '\'' *> (nextP <|> ('\'' <$ strP "\\\'")) <* charP '\'')

cpStringP :: Parser CpExpr
cpStringP = CpString <$> (charP '"' *> spanP (/='"') <* charP '"')

cpIntP :: Parser CpExpr
cpIntP = CpInt <$> (
        strP "0"
    <|> ((:) <$>
        charPredicateP (`elem` numericNo0) <*> spanP (`elem` numeric)))

cpFloatP :: Parser CpExpr
cpFloatP = (\integerPart _ decimalPart -> CpFloat integerPart decimalPart) <$>
    spanP (`elem` numeric) <*> strP "." <*> spanP (`elem` numeric)

cpInBracketP :: Parser CpExpr
cpInBracketP = charP '(' *> manySpaceP *> cpExprP <* manySpaceP <* charP ')'

cpVariableP :: Parser CpExpr
cpVariableP =
    CpVariable <$> ((:)
        <$> charPredicateP (`elem` '_':alpha)
        <*> spanP (`elem` '_':alpha++numeric))

cpFunctionP :: Parser CpExpr
cpFunctionP
    = (\(CpVariable f) _ _ _ params _ _ ->
        case f of {
            -- Built-in functions go here
            "DIV" -> let (x:y:_) = params in CpIntDivide x y;
            "MOD" -> let (x:y:_) = params in CpModulus x y;
            "LENGTH" -> let (string:_) = params in CpBuiltinLength string;
            "LEFT" -> let (string:count:_) = params in CpBuiltinLeft string count;
            "RIGHT" -> let (string:count:_) = params in CpBuiltinRight string count;
            "MID" -> let (string:start:count:_) = params in CpBuiltinMid string start count;
            "NUM_TO_STRING" -> let (expr:_) = params in CpBuiltinNumToString expr;
            -- Not built-in
            _ -> CpFunction f params;
        }
    )
    <$> cpVariableP <*> manySpaceP
    <*> charP '(' <*> manySpaceP
    <*> parametersP <*> manySpaceP
    <*> charP ')'

parametersP :: Parser [CpExpr]
parametersP =
    ((:)
    <$> cpExprP
    <*> many (
            whiteSpaces
        *>  charP ','
        *>  whiteSpaces
        *>  cpExprP
    ))
    <|> [] <$ passP

cpPrimaryP :: Parser CpExpr
cpPrimaryP
    =   cpTrueP
    <|> cpFalseP
    <|> cpCharP
    <|> cpStringP
    <|> cpFloatP
    <|> cpIntP
    <|> cpFunctionP
    <|> cpVariableP
    <|> cpInBracketP

cpIndexP :: Parser CpExpr
cpIndexP =
    foldl (flip($))
    <$> cpPrimaryP
    <*> many (
        (\_ _ _ firstIndex tailIndices _ _ ->
            flip CpIndex (firstIndex:tailIndices))
        <$> manySpaceP
        <*> charP '[' <*> manySpaceP
        <*> cpExprP
        <*> many (manySpaceP *> charP ',' *> manySpaceP *> cpExprP)
        <*> manySpaceP
        <*> charP ']'
    )

cpUnaryP :: Parser CpExpr
cpUnaryP = 
    (\ops expr -> foldr ($) expr (
        (\case {
            "NOT" -> CpNot;
            "-" -> CpNegative;
        }) <$> ops
        )
    )
    <$> many ((strP "NOT" <|> strP "-") <* manySpaceP)
    <*> cpIndexP

cpPowerP :: Parser CpExpr
cpPowerP =
    foldl (flip($)) <$> cpUnaryP <*> many (
        (\_ op _ expr -> flip CpPower expr)
        <$> manySpaceP
        <*> strP "^" <*> manySpaceP
        <*> cpUnaryP
    )

cpFactorP :: Parser CpExpr
cpFactorP =
    foldl (flip($)) <$> cpPowerP <*> many (
        (\_ op _ expr -> case op of {
            "*" -> flip CpMultiply expr;
            "/" -> flip CpDivide expr;
            "MOD" -> flip CpModulus expr;
            "DIV" -> flip CpIntDivide expr;
            _ -> undefined;
        })
        <$> manySpaceP
        <*> (
                strP "*"
            <|> strP "/"
            <|> strP "MOD"
            <|> strP "DIV"
        )
        <*> manySpaceP
        <*> cpPowerP
    )

cpTermP :: Parser CpExpr
cpTermP = 
    foldl (flip($)) <$> cpFactorP <*> many (
        (\_ op _ expr -> case op of {
            "+" -> flip CpAdd expr;
            "&" -> flip CpAdd expr;
            "-" -> flip CpSubtract expr;
            _ -> undefined;
        })
        <$> manySpaceP
        <*> (
                strP "+"
            <|> strP "-"
            <|> strP "&"
        )
        <*> manySpaceP
        <*> cpFactorP
    )

cpCompareP :: Parser CpExpr
cpCompareP = 
    foldl (flip($)) <$> cpTermP <*> many (
        (\_ op _ expr -> case op of {
            -- "< " must have a space or the translator will recognise it as "<-"
            "< " -> flip CpLess expr;
            ">" -> flip CpGreater expr;
            "<=" -> flip CpLessEqual expr;
            ">=" -> flip CpGreaterEqual expr;
            _ -> undefined;
        })
        <$> manySpaceP
        <*> (
                strP "<="
            <|> strP ">="
            <|> strP "< "
            <|> strP ">"
        )
        <*> manySpaceP
        <*> cpTermP
    )

cpEqualityP :: Parser CpExpr
cpEqualityP =
    foldl (flip($)) <$> cpCompareP <*> many (
        (\_ op _ expr -> case op of {
            "=" -> flip CpEqual expr;
            "<>" -> flip CpNotEqual expr;
            _ -> undefined;
        })
        <$> manySpaceP
        <*> (
                strP "="
            <|> strP "<>"
        )
        <*> manySpaceP
        <*> cpCompareP
    )

cpAndP :: Parser CpExpr
cpAndP = foldl (flip($)) <$> cpEqualityP <*> many (
        (\_ op _ expr -> flip CpAnd expr)
        <$> manySpaceP
        <*> strP "AND" <*> manySpaceP
        <*> cpEqualityP
    )

cpOrP :: Parser CpExpr
cpOrP = foldl (flip($)) <$> cpAndP <*> many (
        (\_ op _ expr -> flip CpOr expr)
        <$> manySpaceP
        <*> strP "OR"
        <*> manySpaceP <*> cpAndP
    )

cpExprP :: Parser CpExpr
cpExprP = cpOrP

cpTypeArrayP :: Parser CpType
cpTypeArrayP =
    (\_ _ _ _ headDimension tailDimensions _ _ _ _ _ elementType ->
        CpTypeArray (headDimension : tailDimensions) elementType)
    <$> strP "ARRAY" <*> manySpaceP
    <*> charP '[' <*> manySpaceP
    <*> arrayDimensionP
    <*> many (manySpaceP *> charP ',' *> manySpaceP *> arrayDimensionP)
    <*> manySpaceP
    <*> charP ']' <*> manySpaceP
    <*> strP "OF" <*> manySpaceP
    <*> cpTypeP

arrayDimensionP :: Parser (CpExpr, CpExpr)
arrayDimensionP = 
    (\from _ _ _ to -> (from, to))
    <$> cpExprP <*> manySpaceP
    <*> (charP ':' <|> charP ',') <*> manySpaceP
    <*> cpExprP

cpTypeP :: Parser CpType
cpTypeP =
        CpTypeInteger   <$ strP "INTEGER"
    <|> CpTypeReal      <$ strP "REAL"
    <|> CpTypeBoolean   <$ strP "BOOLEAN"
    <|> CpTypeChar      <$ strP "CHAR"
    <|> CpTypeString    <$ strP "STRING"
    <|> cpTypeArrayP
    <|> CpTypeCustom    <$> cpVariableP


-- Statement parser ------------------------------------------------------------
cpAssignP :: Parser CpStatement
cpAssignP =
    (\var _ _ _ expr -> CpAssign var expr)
    <$> cpExprP <*> manySpaceP
    <*> strP "<-" <*> manySpaceP
    <*> cpExprP

cpOutputP :: Parser CpStatement
cpOutputP =
    (\_ _ params -> CpOutput params)
    <$> strP "OUTPUT" <*> manySpaceP
    <*> parametersP

cpInputP :: Parser CpStatement
cpInputP =
    (\_ _ variable -> CpInput variable)
    <$> strP "INPUT" <*> manySpaceP
    <*> cpExprP

cpReturnP :: Parser CpStatement
cpReturnP =
    (\_ _ expr -> CpReturn expr)
    <$> strP "RETURN" <*> manySpaceP
    <*> cpExprP

cpFunctionCallP :: Parser CpStatement
cpFunctionCallP =
    (\_ _ f -> case f of {
        CpFunction _ _ -> CpFunctionCall f;
        (CpVariable v) -> CpFunctionCall (CpFunction v []);
    })
    <$> strP "CALL" <*> manySpaceP
    <*> (
            cpFunctionP
        <|> cpVariableP
    )

cpBlankLineP :: Parser CpStatement
cpBlankLineP = CpBlankLine <$ manySpaceP

cpDeclareP :: Parser CpStatement
cpDeclareP = 
    (\_ _ variable _ _ _ variableType ->
        CpDeclare variable variableType)
    <$> strP "DECLARE" <*> manySpaceP
    <*> cpVariableP <*> manySpaceP
    <*> charP ':' <*> manySpaceP
    <*> cpTypeP

cpEnumeratedP :: Parser CpStatement
cpEnumeratedP =
    (\_ _ variable _ _ _ _ _ firstConstant _ tailConstants _ _ ->
        CpEnumerated variable (firstConstant : tailConstants))
    <$> strP "TYPE" <*> manySpaceP
    <*> cpVariableP <*> manySpaceP
    <*> charP '=' <*> manySpaceP
    -- If there are too many constants, line breaks are allowed.
    <*> charP '(' <*> whiteSpaces
    <*> cpVariableP <*> whiteSpaces
    <*> many (
            charP ',' *> whiteSpaces
        *>  cpVariableP <* whiteSpaces
    )
    -- Allow a redundant comma after the last constant.
    <*> optional (charP ',' <* whiteSpaces)
    <*> charP ')'

cpFileModeP :: Parser FileMode
cpFileModeP =
        FileRead <$ strP "READ"
    <|> FileWrite <$ strP "WRITE"
    <|> FileAppend <$ strP "APPEND"

cpOpenP :: Parser CpStatement
cpOpenP = 
    (\_ _ path _ _ _ mode -> CpOpen path mode)
    <$> (strP "OPENFILE" <|> strP "OPEN") <*> manySpaceP
    <*> cpExprP <*> manySpaceP
    <*> strP "FOR" <*> manySpaceP
    <*> cpFileModeP

cpWriteP :: Parser CpStatement
cpWriteP =
    (\_ _ path _ _ _ content -> CpWrite path content)
    <$> strP "WRITEFILE" <*> manySpaceP
    <*> cpExprP <*> manySpaceP
    <*> charP ',' <*> manySpaceP
    <*> cpExprP

cpReadP :: Parser CpStatement
cpReadP =
    (\_ _ path _ _ _ variable -> CpRead path variable)
    <$> strP "READFILE" <*> manySpaceP
    <*> cpExprP <*> manySpaceP
    <*> charP ',' <*> manySpaceP
    <*> cpExprP

cpCloseP :: Parser CpStatement
cpCloseP =
    (\_ _ path -> CpClose path)
    <$> strP "CLOSEFILE" <*> manySpaceP
    <*> cpExprP

cpStatementP :: Parser CpStatement
cpStatementP =
    manySpaceP *> (
            cpAssignP
        <|> cpFunctionCallP
        <|> cpOutputP
        <|> cpInputP
        <|> cpReturnP
        <|> cpDeclareP
        <|> cpEnumeratedP
        <|> cpOpenP
        <|> cpWriteP
        <|> cpReadP
        <|> cpCloseP
        <|> cpBlankLineP
    )
    <* manySpaceP
    <* lineBreak


-- Flow parser
cpStatementsP :: Parser CpFlow
cpStatementsP = CpFlow <$> some (CpSingleStatement <$> cpStatementP)

cpIfP :: Parser CpFlow
cpIfP =
    (\_ _ _ condition _ _ _ thenClause _ _ _ _ ->
        CpIf condition thenClause
    )
    <$> manySpaceP
    <*> strP "IF" <*> whiteSpaces
    <*> cpExprP <*> whiteSpaces
    <*> strP "THEN" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> strP "ENDIF" <*> manySpaceP
    <*> lineBreak

cpIfElseP :: Parser CpFlow
cpIfElseP =
    (\_ _ _ condition _ _ _ thenClause _ _ _ elseClause _ _ _ _ ->
        CpIfElse condition thenClause elseClause
    )
    <$> manySpaceP
    <*> strP "IF" <*> whiteSpaces
    <*> cpExprP <*> whiteSpaces
    <*> strP "THEN" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> whiteSpaces
    <*> strP "ELSE" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> strP "ENDIF" <*> manySpaceP
    <*> lineBreak

cpWhileP :: Parser CpFlow
cpWhileP =
    (\_ _ _ condition _ _ _ loopClause _ _ _ _ ->
        CpWhile condition loopClause
    )
    <$> manySpaceP
    <*> strP "WHILE" <*> whiteSpaces
    <*> cpExprP <*> whiteSpaces
    <*> strP "DO" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> strP "ENDWHILE" <*> manySpaceP
    <*> lineBreak

cpRepeatP :: Parser CpFlow
cpRepeatP =
    (\_ _ _ loopClause _ _ _ condition _ _ ->
        CpRepeat loopClause condition)
    <$> manySpaceP
    <*> strP "REPEAT" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> strP "UNTIL" <*> manySpaceP
    <*> cpExprP <*> manySpaceP
    <*> lineBreak

cpForP :: Parser CpFlow
cpForP =
    (\_ _ _ (CpAssign variable from) _ _ _ to _ loopClause _ _ _ _ ->
        CpFor variable from to loopClause)
    <$> manySpaceP
    <*> strP "FOR" <*> manySpaceP
    <*> cpAssignP <*> manySpaceP
    <*> strP "TO" <*> manySpaceP
    <*> cpIntP <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> (strP "NEXT" <|> strP "ENDFOR") <*> manySpaceP
    <*> lineBreak

cpForStepP :: Parser CpFlow
cpForStepP =
    (\_ _ _ (CpAssign variable from) _ _ _ to _  _ _ step _ loopClause _ _ _ _ ->
        CpForStep variable from to step loopClause)
    <$> manySpaceP
    <*> strP "FOR" <*> manySpaceP
    <*> cpAssignP <*> manySpaceP
    <*> strP "TO" <*> manySpaceP
    <*> cpIntP <*> manySpaceP
    <*> strP "STEP" <*> manySpaceP
    <*> cpIntP <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> (strP "NEXT" <|> strP "ENDFOR") <*> manySpaceP
    <*> lineBreak

cpStructP :: Parser CpFlow
cpStructP = 
    (\_ _ _ name _ declareClauses _ _ _ _ -> CpDefineStruct name declareClauses)
    <$> manySpaceP
    <*> strP "TYPE" <*> whiteSpaces
    <*> cpVariableP <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> strP "ENDTYPE" <*> manySpaceP
    <*> lineBreak

cpSignatureProcedure :: Parser [(CpExpr, CpType)]
cpSignatureProcedure =
    (:)
    <$> cpSingleSignatureP
    <*> many (
            whiteSpaces *> charP ',' *> whiteSpaces
        *>  cpSingleSignatureP
    )
    where
    cpSingleSignatureP = 
        (\variable _ _ _ variableType -> (variable, variableType))
        <$> cpVariableP <*> whiteSpaces
        <*> charP ':' <*> whiteSpaces
        <*> cpTypeP

cpDefineProcedure :: Parser CpFlow
cpDefineProcedure =
    (\_ _ _ name _ maybeSignature _ clause _ _ _ _ -> 
        case maybeSignature of {
            Nothing -> CpDefineProcedure  name [] clause;
            _ -> case fromJust maybeSignature of {
                Nothing -> CpDefineProcedure name [] clause;
                _ -> CpDefineProcedure
                        name (fromJust $ fromJust maybeSignature) clause
            }
    })
    <$> manySpaceP
    <*> strP "PROCEDURE" <*> whiteSpaces
    <*> cpVariableP <*> whiteSpaces
    <*> optional (
            charP '(' *> whiteSpaces
        *> optional cpSignatureProcedure <* whiteSpaces
        <* charP ')' <* whiteSpaces
    )
    <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> strP "ENDPROCEDURE" <*> manySpaceP
    <*> lineBreak

cpDefineFunction :: Parser CpFlow
cpDefineFunction =
    (\_ _ _ name _ maybeSignature _ _ _ returnType _ clause _ _ _ _ -> 
        case maybeSignature of {
            Nothing -> CpDefineFunction  name [] returnType clause;
            _ -> case fromJust maybeSignature of {
                Nothing -> CpDefineFunction name [] returnType clause;
                _ -> CpDefineFunction
                        name (fromJust $ fromJust maybeSignature) returnType clause
            }
    })
    <$> manySpaceP
    <*> strP "FUNCTION" <*> whiteSpaces
    <*> cpVariableP <*> whiteSpaces
    <*> optional (
            charP '(' *> whiteSpaces
        *> optional cpSignatureProcedure <* whiteSpaces
        <* charP ')' <* whiteSpaces
    ) <*> whiteSpaces
    <*> strP "RETURNS" <*> whiteSpaces
    <*> cpTypeP <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*> manySpaceP
    <*> strP "ENDFUNCTION" <*> manySpaceP
    <*> lineBreak

cpFlowP :: Parser CpFlow
cpFlowP = CpFlow
    <$> many (
            cpIfP
        <|> cpIfElseP
        <|> cpWhileP
        <|> cpRepeatP
        <|> cpForP
        <|> cpForStepP
        <|> cpStructP
        <|> cpDefineProcedure
        <|> cpDefineFunction
        <|> cpStatementsP
    )


-- Dump to Python --------------------------------------------------------------
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


translate :: String -> Maybe String
translate pseudocode = do
    (_, program) <- run cpFlowP pseudocode
    let (_, output) = dump (initialState, program)
    return output

-- Main function for testing only ----------------------------------------------
main :: IO ()
main = do
    let inputPath = "test.campseudo"
        outputPath = inputPath ++ ".py"

    raw <- readFile inputPath
    print $ translate raw
