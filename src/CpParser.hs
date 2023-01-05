{-# LANGUAGE LambdaCase #-}

module CpParser (
    identifier,
    cpFlowP,
) where

import CpAdt
import MyParser

import Control.Applicative (optional, many, some, (<|>), Alternative (empty))
import Data.Maybe (fromJust, isJust)


alphaUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
alphaLower = "abcdefghijklmnopqrstuvwxyz"
alpha = alphaUpper ++ alphaLower
numeric = "0123456789"
numericNo0 = "123456789"
identifier = alpha ++ numeric ++ "_"


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
            -- Infix operators
            "DIV" -> let (x:y:_) = params in CpIntDivide x y;
            "MOD" -> let (x:y:_) = params in CpModulus x y;
            -- Built-in functions
            "CHR" -> let (order:_) = params in CpBuiltinChr order;
            "ASC" -> let (char:_) = params in CpBuiltinAsc char;
            "LCASE" -> let (string:_) = params in CpBuiltinLcase string;
            "UCASE" -> let (string:_) = params in CpBuiltinUcase string;
            "TO_UPPER" -> let (string:_) = params in CpBuiltinToUpper string;
            "TO_LOWER" -> let (string:_) = params in CpBuiltinToLower string;
            "LENGTH" -> let (string:_) = params in CpBuiltinLength string;
            "LEFT" -> let (string:count:_) = params in CpBuiltinLeft string count;
            "RIGHT" -> let (string:count:_) = params in CpBuiltinRight string count;
            "MID" -> let (string:start:count:_) = params in CpBuiltinMid string start count;
            "NUM_TO_STRING" -> let (expr:_) = params in CpBuiltinNumToString expr;
            -- Special built-in functions
            "EOF" -> let (path:_) = params in CpBuiltinEof path;
            "RAND" -> CpBuiltinRand;
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

cpTakeAttributeOrIndexP :: Parser CpExpr
cpTakeAttributeOrIndexP =
    foldl (flip($)) <$> cpPrimaryP <*> many (
        -- take attribute
        ((\_ _ _ expr -> flip CpTakeAttribute expr)
            <$> manySpaceP
            <*> strP "."
            <*> manySpaceP
            <*> cpPrimaryP)
        <|>
        -- index
        ((\_ _ _ firstIndex tailIndices _ _ ->
            flip CpIndex (firstIndex:tailIndices))
        <$> manySpaceP
        <*> charP '[' <*> manySpaceP
        <*> cpExprP
        <*> many (manySpaceP *> charP ',' *> manySpaceP *> cpExprP)
        <*> manySpaceP
        <*> charP ']')
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
    <*> cpTakeAttributeOrIndexP

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
            <|> strP "< " -- So is here
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
        <*> (strP "=" <|> strP "<>")
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
    <*> optional(strP "DO" <* manySpaceP) <*> optional lineBreak
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
