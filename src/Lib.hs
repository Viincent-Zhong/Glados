module Lib (
        Cpt (..),
        Ast (..),
        Env,
        Instruction (..),
        Stack,
        Function,
        insertToTupleArray,
        addToTupleArray,
        searchTupleArray,
        removeFromTupleArray,
        addMultipleToTupleArray
    ) where

data Ast = AstInteger Int
    | AstSymbol String
    | AstBoolean String
    | AstDefine (Either String [String]) Ast
    | AstCall [Ast]
    | AstLambda [String] Ast
    -- deriving (Show)

type Env = [(String, Either ([String], [Instruction]) Ast)]
type Stack = [Ast]
type Function = Stack -> Either Stack String

instance Show Ast where
    show (AstInteger n) = show n
    show (AstSymbol n) = n
    show (AstBoolean "#f") = "false"
    show (AstBoolean "#t") = "true"
    show (AstBoolean n) = n
    show (AstCall []) = "<function>"
    show (AstCall (s:_)) = "<function>" ++ show s
    show (AstDefine (Left s) n) = s ++ " " ++ show n
    show (AstDefine (Right []) n) = show n
    show (AstDefine (Right (s:_)) n) = s ++ " " ++ show n
    show (AstLambda [] n) = show n
    show (AstLambda (s:_) n) = s ++ " " ++ show n

instance Eq Ast where
    (AstInteger n1) == (AstInteger n2) = n1 == n2
    (AstSymbol n1) == (AstSymbol n2) = n1 == n2
    (AstBoolean n1) == (AstBoolean n2) = n1 == n2
    (AstCall s1) == (AstCall s2) = s1 == s2
    (AstDefine s1 n1) == (AstDefine s2 n2) = s1 == s2 && n1 == n2
    -- (AstDefine (Left s1) n1) == (AstDefine (Left s2) n2) = s1 == s2 && n1 == n2
    -- (AstDefine (Right s1) n1) == (AstDefine (Right s2) n2) = s1 == s2 && n1 == n2
    (AstLambda s1 n1) == (AstLambda s2 n2) = s1 == s2 && n1 == n2
    _ == _ = False

data Cpt = CptLists [Cpt]
    | CptSymbols String
    | CptInteger Int deriving (Show)

-- instance Show Cpt where
    -- show (CptLists (a : b)) = show a ++ show b
    -- show (CptLists []) = ""
    -- show (CptSymbols s) = show s
    -- show (CptInteger i) = show i

instance Eq Cpt where
    (CptLists (a1 : b1)) == (CptLists (a2 : b2)) = a1 == a2 && b1 == b2
    (CptLists []) == (CptLists []) = True
    (CptSymbols s1) == (CptSymbols s2) = s1 == s2
    (CptInteger i1) == (CptInteger i2) = i1 == i2
    _ == _ = False

data Instruction = Instruction {
    line :: Int,
    command :: String,
    value :: Maybe Ast
} deriving (Show, Eq)

insertToTupleArray :: [(String, a)] -> String -> a -> [(String, a)]
insertToTupleArray list str v = case lookup str list of
    Nothing -> (str, v) : list
    Just _ -> replaceKey list
        where
            replaceKey [] = []
            replaceKey ((s, val) : b) | s == str = ((str, v) : b)
                                    | otherwise = ((s, val) : (replaceKey b))

addToTupleArray :: [(String, a)] -> String -> a -> [(String, a)]
addToTupleArray list str a = (str, a) : list

addMultipleToTupleArray :: [(String, a)] -> [String] -> [a] -> [(String, a)]
addMultipleToTupleArray list [] [] = list
addMultipleToTupleArray list (str:ns) (a:na) = addMultipleToTupleArray ((str, a) : list) ns na
addMultipleToTupleArray list _ _ = list

searchTupleArray :: [(String, a)] -> String -> Maybe a
searchTupleArray list str = lookup str list

removeFromTupleArray :: [(String, a)] -> String -> [(String, a)]
removeFromTupleArray [] _ = []
removeFromTupleArray ((name, val):b) s = case s == name of
    True -> b
    False -> (name, val) : removeFromTupleArray b s
