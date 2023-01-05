module CpAdt (
    CpExpr(..),
    CpType(..),
    FileMode(..),
    CpStatement(..),
    CpFlow(..),
    getSpecial,
    getImplementation,
) where


import Data.Data (Typeable, Data (toConstr), Constr)
import Data.List (nub, intercalate)


-- Syntax tree

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
    | CpTakeAttribute CpExpr CpExpr

    -- Built-in functions
    | CpBuiltinChr CpExpr
    | CpBuiltinAsc CpExpr
    | CpBuiltinLcase CpExpr
    | CpBuiltinUcase CpExpr
    | CpBuiltinToUpper CpExpr
    | CpBuiltinToLower CpExpr
    | CpBuiltinLength CpExpr
    | CpBuiltinLeft CpExpr CpExpr
    | CpBuiltinRight CpExpr CpExpr
    | CpBuiltinMid CpExpr CpExpr CpExpr
    | CpBuiltinNumToString CpExpr
    -- Special built-in functions
    | CpBuiltinEof CpExpr
    | CpBuiltinRand
    deriving (Show, Data, Typeable)

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


-- Priority

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
    priority (CpTakeAttribute{}) = 1
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


{-
Some functions in pseudocode are very special (e.g. EOF and RAND).
For the function EOF, it requires a explicit reimplementation in Python.
For the function RAND, "random" module is required.
-}

constrEof = toConstr (CpBuiltinEof undefined)
constrRand = toConstr CpBuiltinRand

getImplementationPath :: Constr -> String
getImplementationPath constr
    | constr == constrEof = "special-EOF.py"
    | constr == constrRand = "special-RAND.py"
    | otherwise = undefined

getImplementation :: Constr -> String
getImplementation constr
    | constr == constrEof = intercalate "\n" [
        "from io import TextIOWrapper",
        "",
        "def EOF(f: TextIOWrapper) -> bool:",
        "    last_seek = f.seek(0, 1)",
        "    if f.readline():",
        "        f.seek(last_seek)",
        "        return False",
        "    return True\n"]
    | constr == constrRand =
        "from random import random\n"
    | otherwise = undefined

class GetSpecialFunctions a where
    getSpecial :: a -> [Constr]

instance GetSpecialFunctions CpExpr where
    getSpecial :: CpExpr -> [Constr]
    -- EOF(path : STRING) RETURNS BOOLEAN
    -- RAND() RETURNS REAL
    getSpecial expr@(CpBuiltinEof{}) = [toConstr expr]
    
    getSpecial (CpNot expr1) = getSpecial expr1
    getSpecial (CpAnd expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpOr expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2

    getSpecial expr@(CpBuiltinRand{}) = [toConstr expr]

    getSpecial (CpNegative expr1) = getSpecial expr1
    getSpecial (CpPower expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpMultiply expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpDivide expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpModulus expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpIntDivide expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpAdd expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpSubtract expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpLess expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpGreater expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpLessEqual expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpGreaterEqual expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpEqual expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2
    getSpecial (CpNotEqual expr1 expr2) = nub $
        getSpecial expr1 ++ getSpecial expr2

    getSpecial _ = []

instance GetSpecialFunctions CpStatement where
    getSpecial :: CpStatement -> [Constr]
    getSpecial (CpAssign _ value) = getSpecial value
    getSpecial (CpOutput values) = nub $ concatMap getSpecial values
    getSpecial (CpFunctionCall call) = getSpecial call
    getSpecial (CpReturn value) = getSpecial value

    getSpecial _ = []


instance GetSpecialFunctions CpFlow where
    getSpecial :: CpFlow -> [Constr]
    getSpecial (CpFlow flows) =
        nub $ concatMap getSpecial flows
    getSpecial (CpSingleStatement statement) =
        nub $ getSpecial statement
    getSpecial (CpIf condition thenClause) =
        nub $ getSpecial condition ++ getSpecial thenClause
    getSpecial (CpIfElse condition thenClause elseClause) =
        nub $
            getSpecial condition ++
            getSpecial thenClause ++
            getSpecial elseClause
    getSpecial (CpWhile condition loopClause) =
        nub $ getSpecial condition ++ getSpecial loopClause
    getSpecial (CpRepeat loopClause condition) =
        nub $ getSpecial condition ++ getSpecial loopClause
    getSpecial (CpFor _ _ _ loopClause) = getSpecial loopClause
    getSpecial (CpForStep _ _ _ _ loopClause) = getSpecial loopClause
    getSpecial (CpDefineProcedure _ _ body) = getSpecial body
    getSpecial (CpDefineFunction _ _ _ body) = getSpecial body

    getSpecial _ = []
