{-# LANGUAGE LambdaCase #-}
module CambridgePseudocodeToPython (cpFlowP, dump, initialState) where

import MyParser
import Control.Applicative (optional, many, some, (<|>))
import Debug.Trace (trace)
import Data.List (intercalate, intersperse)

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing, isJust)
import System.Environment (getArgs)


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
    priority (CpNegative _) = 2
    priority (CpPower _ _) = 3
    priority (CpMultiply _ _) = 4
    priority (CpDivide _ _) = 4
    priority (CpModulus _ _) = 4
    priority (CpIntDivide _ _) = 4
    priority (CpAdd _ _) = 5
    priority (CpSubtract _ _) = 5
    priority (CpLess _ _) = 6
    priority (CpGreater _ _) = 6
    priority (CpLessEqual _ _) = 6
    priority (CpGreaterEqual _ _) = 6
    priority (CpEqual _ _) = 7
    priority (CpNotEqual _ _) = 7
    priority (CpAnd _ _) = 8
    priority (CpOr _ _) = 9
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
    deriving Show

data CpType
    = CpTypeInteger
    | CpTypeReal
    | CpTypeBoolean
    | CpTypeString
    | CpTypeChar
    | CpTypeArray [(CpExpr, CpExpr)] CpType
    deriving Show
    
data CpStatement
    = CpAssign CpExpr CpExpr
    | CpDeclare CpExpr CpType
    | CpInput CpExpr
    | CpOutput [CpExpr]
    | CpFunctionCall CpExpr
    | CpReturn CpExpr
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
    = (\(CpVariable f) _ _ _ params _ _ -> case f of {
        -- Built-in functions go here
        "DIV" -> let (x:y:_) = params in CpIntDivide x y;
        "MOD" -> let (x:y:_) = params in CpModulus x y;
        -- Not built-in
        _ -> CpFunction f params;
    })
    <$> cpVariableP
    <*> manySpaceP
    <*> charP '('
    <*> manySpaceP
    <*> parametersP
    <*> manySpaceP
    <*> charP ')'

parametersP :: Parser [CpExpr]
parametersP = (:)
    <$> cpExprP
    <*> many (
            manySpaceP
        *>  charP ','
        *>  manySpaceP
        *>  cpExprP
    )

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
        <*> many (manySpaceP *> charP ',' *> manySpaceP *> cpExprP) <*> manySpaceP
        <*> charP ']')


cpUnaryP :: Parser CpExpr
cpUnaryP = 
    (\ops expr -> foldr ($) expr (
        (\case {
            "NOT" -> CpNot;
            "-" -> CpNegative;
        }) <$> ops
        )
    ) <$> many ((strP "NOT" <|> strP "-") <* manySpaceP) <*> cpIndexP

cpPowerP :: Parser CpExpr
cpPowerP =
    foldl (flip($)) <$> cpUnaryP <*> many (
        (\_ op _ expr -> flip CpPower expr)
        <$> manySpaceP <*> strP "^" <*> manySpaceP <*> cpUnaryP
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
        }) <$> manySpaceP
        <*> (strP "*" <|> strP "/" <|> strP "MOD" <|> strP "DIV")
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
        }) <$> manySpaceP
        <*> (strP "+" <|> strP "-" <|> strP "&")
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
        }) <$> manySpaceP
        <*> (strP "<=" <|> strP ">=" <|> strP "< " <|> strP ">") <*> manySpaceP
        <*> cpTermP
    )

cpEqualityP :: Parser CpExpr
cpEqualityP =
    foldl (flip($)) <$> cpCompareP <*> many (
        (\_ op _ expr -> case op of {
            "=" -> flip CpEqual expr;
            "<>" -> flip CpNotEqual expr;
            _ -> undefined;
        }) <$> manySpaceP <*> (strP "=" <|> strP "<>") <*> manySpaceP <*> cpCompareP
    )

cpAndP :: Parser CpExpr
cpAndP = foldl (flip($)) <$> cpEqualityP <*> many (
        (\_ op _ expr -> flip CpAnd expr)
        <$> manySpaceP <*> strP "AND" <*> manySpaceP <*> cpEqualityP
    )

cpOrP :: Parser CpExpr
cpOrP = foldl (flip($)) <$> cpAndP <*> many (
        (\_ op _ expr -> flip CpOr expr)
        <$> manySpaceP <*> strP "OR" <*> manySpaceP <*> cpAndP
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
    <*> many (manySpaceP *> charP ',' *> manySpaceP *> arrayDimensionP) <*> manySpaceP
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
    <$> strP "INPUT"
    <*> manySpaceP
    <*> cpExprP

cpFunctionCallP :: Parser CpStatement
cpFunctionCallP =
    (\_ _ f -> CpFunctionCall f)
    <$> strP "CALL"
    <*> manySpaceP
    <*> cpFunctionP

cpBlankLineP :: Parser CpStatement
cpBlankLineP = CpBlankLine <$ manySpaceP

cpDeclareP :: Parser CpStatement
cpDeclareP = 
    (\_ _ variable _ _ _ variableType ->
        CpDeclare variable variableType)
    <$> strP "DECLARE"
    <*> manySpaceP
    <*> cpVariableP
    <*> manySpaceP <*> charP ':' <*> manySpaceP
    <*> cpTypeP

cpStatementP :: Parser CpStatement
cpStatementP =
    manySpaceP *> (
        cpAssignP
    <|> cpFunctionCallP
    <|> cpOutputP
    <|> cpInputP
    <|> cpDeclareP
    <|> cpBlankLineP
    )
    <* manySpaceP
    <* lineBreak


-- Flow parser
cpStatementsP :: Parser CpFlow
cpStatementsP = CpFlow <$> some (CpSingleStatement <$> cpStatementP)

cpIfP :: Parser CpFlow
cpIfP = (\_ _ _ condition _ _ _ thenClause _ _ _ _ ->
        CpIf condition thenClause
    )
    <$>
    manySpaceP <*> strP "IF" <*> whiteSpaces
    <*> cpExprP <*>
    whiteSpaces <*> strP "THEN" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*>
    manySpaceP <*> strP "ENDIF" <*> manySpaceP
    <*> lineBreak

cpIfElseP :: Parser CpFlow
cpIfElseP = (\_ _ _ condition _ _ _ thenClause _ _ _ elseClause _ _ _ _ ->
        CpIfElse condition thenClause elseClause
    )
    <$>
    manySpaceP <*> strP "IF" <*> whiteSpaces
    <*> cpExprP <*>
    whiteSpaces <*> strP "THEN" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*>
    whiteSpaces <*> strP "ELSE" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*>
    manySpaceP <*> strP "ENDIF" <*> manySpaceP
    <*> lineBreak

cpWhileP :: Parser CpFlow
cpWhileP = (\_ _ _ condition _ _ _ loopClause _ _ _ _ ->
        CpWhile condition loopClause
    )
    <$>
    manySpaceP <*> strP "WHILE" <*> whiteSpaces
    <*> cpExprP <*>
    whiteSpaces <*> strP "DO" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*>
    manySpaceP <*> strP "ENDWHILE" <*> manySpaceP
    <*> lineBreak

cpRepeatP :: Parser CpFlow
cpRepeatP = (\_ _ _ loopClause _ _ _ condition _ _ ->
        CpRepeat loopClause condition)
    <$>
    manySpaceP <*> strP "REPEAT" <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*>
    manySpaceP <*> strP "UNTIL" <*> manySpaceP <*> cpExprP <*> manySpaceP
    <*> lineBreak

cpForP :: Parser CpFlow
cpForP = (\_ _ _ (CpAssign variable from) _ _ _ to _ loopClause _ _ _ _ ->
        CpFor variable from to loopClause)
    <$>
    manySpaceP <*> strP "FOR" <*> manySpaceP <*> cpAssignP <*> manySpaceP
    <*> strP "TO" <*> manySpaceP <*> cpIntP <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*>
    manySpaceP <*> (strP "NEXT" <|> strP "ENDFOR") <*> manySpaceP
    <*> lineBreak

cpForStepP :: Parser CpFlow
cpForStepP = (\_ _ _ (CpAssign variable from) _ _ _ to _  _ _ step _ loopClause _ _ _ _ ->
        CpForStep variable from to step loopClause)
    <$>
    manySpaceP <*> strP "FOR" <*> manySpaceP <*> cpAssignP <*> manySpaceP
    <*> strP "TO" <*> manySpaceP <*> cpIntP <*> manySpaceP
    <*> strP "STEP" <*> manySpaceP <*> cpIntP <*> (manySpaceP <* optional lineBreak)
    <*> cpFlowP <*>
    manySpaceP <*> (strP "NEXT" <|> strP "ENDFOR") <*> manySpaceP
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
        <|> cpStatementsP
    )


-- Dump to Python --------------------------------------------------------------
class DumpPython program where
    dump :: (State, program) -> (State, String)
    -- dump (Ignore Indentation)
    
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
                        arrayType@(CpTypeArray dimensions _)
                    )
                ):tail)) =
        (   state,
            indent indentation ++ declarationOutput ++ "\n" ++
            indent indentation ++ initialisationOutput ++ "\n" ++
            nextOutput)
        where
            declarationOutput = dumpE variable ++ ": " ++ dumpE arrayType
            initialisationOutput =
                dumpE variable ++
                " = " ++
                concat (replicate (length dimensions) "[") ++
                if length dimensions == 1 then
                    let (from, to) = head dimensions in
                    "... for _ in range(" ++
                    dumpE from ++
                    ", " ++
                    dumpE to ++ "+1)]"
                else
                    -- If this is an multi-dimensional array,
                    -- add a line break for each dimension
                    "...\n" ++
                    intercalate "\n" (
                        (\(from, to) ->
                            indent (indentation+1) ++
                            "for _ in range(" ++
                                dumpE from ++
                                ", " ++
                                dumpE to ++ "+1)]")
                        <$> reverse dimensions)

            (nextState, nextOutput) = dump (state, CpFlow tail)

    dump (state@(State indentation),
          CpFlow ((CpSingleStatement statement):tail)) =
        (state, indent indentation ++ output ++ "\n" ++ nextOutput)
        where
            output = dumpE statement
            (nextState, nextOutput) = dump (state, CpFlow tail)
    
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


instance DumpPythonStateless CpStatement where
    dumpE :: CpStatement -> String
    dumpE (CpOutput exprs) = output
        where
            content = intercalate ", " $ dumpE <$> exprs
            output = "print(" ++ content ++ ")"

    dumpE (CpInput expr) = output
        where
            variable = dumpE expr
            output = variable ++ " = input()"
        
    dumpE (CpAssign expr1 expr2) = output
        where
            variable = dumpE expr1
            value = dumpE expr2
            output = variable ++ " = " ++ value
    
    dumpE (CpFunctionCall expr) = output where
        output = dumpE expr

    dumpE (CpDeclare variable variableType) = output where
        output = dumpE variable ++ ": " ++ dumpE variableType

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

    dumpE expr@(CpIndex expr1 indices) = output where
        output =
            dumpE expr1 ++
            concatMap (\index -> "[" ++ dumpE index ++ "]") indices

    dumpE expr@(CpNot expr1) = "not " ++ encloseBracket expr expr1 (dumpE expr1)
    dumpE (CpNegative expr) = "-" ++ dumpE expr

    dumpE expr@(CpPower expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " ** " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpMultiply expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " * " ++
            encloseBracket expr expr2 right
    
    dumpE expr@(CpDivide expr1 expr2@(CpMultiply _ _)) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " / (" ++ right ++ ")"

    dumpE expr@(CpDivide expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " / " ++
            encloseBracket expr expr2 right
            
    dumpE expr@(CpIntDivide expr1 expr2@(CpMultiply _ _)) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " // (" ++ right ++ ")"

    dumpE expr@(CpIntDivide expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " // " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpModulus expr1 expr2@(CpMultiply _ _)) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " % (" ++ right ++ ")"

    dumpE expr@(CpModulus expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output = 
            encloseBracket expr expr1 left ++
            " % " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpAdd expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " + " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpSubtract expr1 expr2@(CpAdd _ _)) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " - (" ++ right ++ ")"

    dumpE expr@(CpSubtract expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " - " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpLess expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " < " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpGreater expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " > " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpLessEqual expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " <= " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpGreaterEqual expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " >= " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpEqual expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " == " ++
            encloseBracket expr expr2 right
    
    dumpE expr@(CpNotEqual expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " != " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpAnd expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " and " ++
            encloseBracket expr expr2 right

    dumpE expr@(CpOr expr1 expr2) = output where
        left = dumpE expr1
        right = dumpE expr2
        output =
            encloseBracket expr expr1 left ++
            " or " ++
            encloseBracket expr expr2 right

encloseBracket :: Ord a => a -> a -> String -> String
encloseBracket outside inside dumpedInside =
    if outside >= inside
    then dumpedInside
    else "(" ++ dumpedInside ++ ")"

instance DumpPythonStateless CpType where
    dumpE CpTypeString = "str"
    dumpE CpTypeInteger = "int"
    dumpE CpTypeReal = "float"
    dumpE CpTypeBoolean = "bool"
    dumpE CpTypeChar = "str"
    dumpE (CpTypeArray shape elementType) =
        concat (replicate (length shape) "list[") ++
        dumpE elementType ++
        concat (replicate (length shape) "]")


newtype State = State (Int) deriving Show
initialState :: State
initialState = State (0)


main :: IO ()
main = do
    let inputPath = "test.ciepseudo"
        outputPath = inputPath ++ ".py"

    raw <- readFile inputPath
    let programMaybe = run cpFlowP raw

    if isJust programMaybe then do
        let program = snd $ fromJust programMaybe 
        let output = snd $ dump (initialState, program)
        print program
        writeFile outputPath output
        putStrLn $ "Complete. File generated at \"" ++ outputPath ++ "\""
    else do
        putStrLn "Syntax error(s) exists."
