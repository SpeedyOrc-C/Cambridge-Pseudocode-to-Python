{-# LANGUAGE LambdaCase #-}
module CambridgePseudocodeToPython () where

import MyParser
import Control.Applicative (optional, many, some, (<|>))
import Debug.Trace (trace)
import Control.Monad (when)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (intercalate, intersperse)


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
    priority (CpNot _) = 1
    priority (CpNegative _) = 1
    priority (CpPower _ _) = 2
    priority (CpMultiply _ _) = 3
    priority (CpDivide _ _) = 3
    priority (CpModulus _ _) = 3
    priority (CpIntDivide _ _) = 3
    priority (CpAdd _ _) = 4
    priority (CpSubtract _ _) = 4
    priority (CpLess _ _) = 5
    priority (CpGreater _ _) = 5
    priority (CpLessEqual _ _) = 5
    priority (CpGreaterEqual _ _) = 5
    priority (CpEqual _ _) = 6
    priority (CpNotEqual _ _) = 6
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
    deriving Show

data CpType
    = CpTypeInteger
    | CpTypeReal
    | CpTypeBoolean
    | CpTypeString
    | CpTypeChar
    | CpTypeArray String String CpType
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
    | CpDoWhile CpFlow CpExpr
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
cpVariableP = CpVariable <$> variableStrP

variableStrP :: Parser [Char]
variableStrP = (:) <$>
    charPredicateP (`elem` ':':alpha) <*> spanP (`elem` ':':alpha++numeric)
    
cpFunctionP :: Parser CpExpr
cpFunctionP
    = (\f _ _ _ params _ _ -> case f of {
        -- Built-in functions go here
        "DIV" -> let (x:y:_) = params in CpIntDivide x y;
        "MOD" -> let (x:y:_) = params in CpModulus x y;
        -- Not built-in
        _ -> CpFunction f params;
    })
    <$> variableStrP
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


cpUnaryP :: Parser CpExpr
cpUnaryP = 
    (\ops expr -> foldr ($) expr (
        (\case {
            "NOT" -> CpNot;
            "-" -> CpNegative;
        }) <$> ops
        )
    ) <$> many ((strP "NOT" <|> strP "-") <* manySpaceP) <*> cpPrimaryP

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
            "-" -> flip CpSubtract expr;
            _ -> undefined;
        }) <$> manySpaceP
        <*> (strP "+" <|> strP "-")
        <*> manySpaceP
        <*> cpFactorP
    )

cpCompareP :: Parser CpExpr
cpCompareP = 
    foldl (flip($)) <$> cpTermP <*> many (
        (\_ op _ expr -> case op of {
            "<" -> flip CpLess expr;
            ">" -> flip CpGreater expr;
            "<=" -> flip CpLessEqual expr;
            ">=" -> flip CpGreaterEqual expr;
            _ -> undefined;
        }) <$> manySpaceP <*> (strP "<=" <|> strP ">=" <|> strP "<" <|> strP ">") <*> manySpaceP <*> cpTermP
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


-- Statement parser ------------------------------------------------------------
cpAssignP :: Parser CpStatement
cpAssignP =
    (\var _ _ _ expr -> CpAssign var expr)
    <$> cpVariableP
    <*> manySpaceP
    <*> strP "<-"
    <*> manySpaceP
    <*> cpExprP

cpOutputP :: Parser CpStatement
cpOutputP =
    (\_ _ params -> CpOutput params)
    <$> strP "OUTPUT"
    <*> manySpaceP
    <*> parametersP

cpInputP :: Parser CpStatement
cpInputP =
    (\_ _ variable -> CpInput (CpVariable variable))
    <$> strP "INPUT"
    <*> manySpaceP
    <*> variableStrP

cpFunctionCallP :: Parser CpStatement
cpFunctionCallP =
    (\_ _ f -> CpFunctionCall f)
    <$> strP "CALL"
    <*> manySpaceP
    <*> cpFunctionP

cpBlankLineP :: Parser CpStatement
cpBlankLineP = CpBlankLine <$ manySpaceP

cpStatementP :: Parser CpStatement
cpStatementP =
    manySpaceP *> (
        cpAssignP
    <|> cpFunctionCallP
    <|> cpOutputP
    <|> cpInputP
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


cpFlowP :: Parser CpFlow
cpFlowP = CpFlow
    <$> many (
            cpIfP
        <|> cpIfElseP
        <|> cpWhileP
        <|> cpStatementsP
    )


-- Dump to Python --------------------------------------------------------------
class DumpPython program where
    dump :: (TranslatorState, program) -> (TranslatorState, String)
    -- dump (Ignore Indentation)
    
class DumpPythonIgnoreIndentation expr where
    dumpE :: expr -> String

indent :: Int -> String
indent n = replicate (4*n) ' '

instance DumpPython CpFlow where
    dump :: (TranslatorState, CpFlow) -> (TranslatorState, String)
    dump (state, CpFlow []) = (state, "")

    dump (state, CpFlow (head@(CpFlow _) : tail)) =
        (next2state, output ++ next2output)
        where
            (nextState, output) = dump (state, head)
            (next2state, next2output) = dump (nextState, CpFlow tail)


    dump (state@(TranslatorState indentation),
          CpFlow ((CpSingleStatement statement):tail)) =
        (state, indent indentation ++ output ++ "\n" ++ nextOutput)
        where
            output = dumpE statement
            (nextState, nextOutput) = dump (state, CpFlow tail)
    
    dump (  state@(TranslatorState indentation),
            CpFlow ((CpIf condition thenClause):tail)) =
        (state, output)
        where
            (_, thenClauseOutput) =
                dump (TranslatorState (indentation+1), thenClause)
            (_, afterEndIfOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "if " ++ dumpE condition ++ ":\n" ++
                    thenClauseOutput ++
                afterEndIfOutput

    dump (  state@(TranslatorState indentation),
            CpFlow ((CpIfElse condition thenClause elseClause):tail)) =
        (state, output)
        where
            (_, thenClauseOutput) =
                dump (TranslatorState (indentation+1), thenClause)
            (_, elseClauseOutput) = 
                dump (TranslatorState (indentation+1), elseClause)
            (_, afterEndIfOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "if " ++ dumpE condition ++ ":\n" ++
                    thenClauseOutput ++
                indent indentation ++ "else:\n" ++
                    elseClauseOutput ++
                afterEndIfOutput

    dump (  state@(TranslatorState indentation),
            CpFlow ((CpWhile condition loopClause):tail)) =
        (state, output)
        where
            (_, loopClauseOutput) =
                dump (TranslatorState (indentation+1), loopClause)
            (_, afterEndwhileOutput) = dump (state, CpFlow tail)

            output =
                indent indentation ++ "while " ++ dumpE condition ++ ":\n" ++
                    loopClauseOutput ++
                afterEndwhileOutput


instance DumpPythonIgnoreIndentation CpStatement where
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

    dumpE CpBlankLine = ""

instance DumpPythonIgnoreIndentation CpExpr where
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

encloseBracket :: Ord a => a -> a -> String -> String
encloseBracket outside inside dumpedInside =
    if outside >= inside
    then dumpedInside
    else "(" ++ dumpedInside ++ ")"


newtype TranslatorState = TranslatorState (Int) deriving Show
initialTranslatorState :: TranslatorState
initialTranslatorState = TranslatorState (0)


main = do
    raw <- readFile "test.ciepseudo"
    let programMaybe = run cpFlowP raw
    when (isJust programMaybe) $ do
        let raw@(_, program) = fromJust programMaybe 
        print raw
        let (_, output) = dump (initialTranslatorState, program)
        writeFile "test.ciepseudo.py" output
