module Eval
    (
        exec,
        add,
        minus,
        mult,
        division,
        modulo,
        inferiorto,
        equal,
        setArgToEnv,
        jump,
        deleteEnv,
        stackPush,
        stackPop,
        extractNFromList,
        removeNFromList
    ) where

import Lib

setArgToEnv :: [String] -> Stack -> Env -> Env
setArgToEnv [] [] env = env
setArgToEnv [] _ env = env
setArgToEnv _ [] env = env
setArgToEnv (name:ns) (val:vs) env = setArgToEnv ns vs (addToTupleArray env name (Right val))

-- In case of success return (extracted values, new stack)
extractNFromList :: [a] -> Int -> Either ([a], [a]) String
extractNFromList l n = case length l >= n of
    True -> Left (take n l, drop n l)
    False -> Right "Not enough elements from list"

removeNFromList ::  [a] -> Int -> Either [a] String
removeNFromList l n = case length l >= n of
    True -> Left (drop n l)
    False -> Right "Not enough elements from list"

stackPush :: Stack -> Ast -> Stack
stackPush a v = v : a

stackPop :: Stack -> Stack
stackPop [] = []
stackPop (_:b) = b

jump :: [Instruction] -> Int -> [Instruction]
jump [] _ = []
jump (x:xs) lineNum
    | line x == lineNum = x:xs
    | otherwise = jump xs lineNum

push :: Ast -> Stack -> Either Stack String
push ast stack = case ast of
    (AstInteger _) -> Left (ast:stack)
    (AstBoolean _) -> Left (ast:stack)
    (_) -> Right "Error in push"

-------------------------------- BUILTINS --------------------------------

add :: Function
add (AstInteger a : AstInteger b : rest) = Left (AstInteger (b + a) : rest)
add _ = Right "+ invalid function call"


minus :: Function
minus (AstInteger a : AstInteger b : rest) = Left (AstInteger (b - a) : rest)
minus _ = Right "- invalid function call"


mult :: Function
mult (AstInteger a : AstInteger b : rest) = Left (AstInteger (b * a) : rest)
mult _ = Right "* invalid function call"


division :: Function
division (AstInteger a : AstInteger b : rest)
    | a == 0 = Right "div divide by zero"
    | otherwise = Left (AstInteger (b `div` a) : rest)
division _ = Right "div invalid function call"


modulo :: Function
modulo (AstInteger a : AstInteger b : rest)
    | a == 0 = Right "mod divide by zero"
    | otherwise = Left (AstInteger (b `mod` a) : rest)
modulo _ = Right "mod invalid function call"


inferiorto :: Function
inferiorto (AstInteger a : AstInteger b : rest)
    | b < a = Left (AstBoolean "#t" : rest)
    | otherwise = Left (AstBoolean "#f" : rest)
inferiorto _ = Right "< invalid function call"

equal :: Function
equal (a:b:rest) = case a == b of
    True -> Left (AstBoolean "#t" : rest)
    False -> Left (AstBoolean "#f" : rest)
equal _ = Right "eq? invalid function call"

getBuiltins :: [(String, Function)]
getBuiltins = [
        ("+", add),
        ("-", minus),
        ("*", mult),
        ("div", division),
        ("mod", modulo),
        ("<", inferiorto),
        ("eq?", equal)
    ]

isBuiltin :: Stack -> (Int, String) -> (Bool, Either Stack String)
isBuiltin stack (_, s) = case searchTupleArray getBuiltins s of
    Nothing -> (False, Left stack)
    Just built -> (True, built stack)

isFunc :: (Int, String) -> Stack -> Env -> Either Stack String
isFunc (_, s) stack env = case searchTupleArray env s of
    Nothing -> Right (s ++ " undefined function")
    Just (Left (args, instructions)) -> case extractNFromList stack (length args) of
        Right err -> Right err
        Left (tempstack, newstack) -> case exec instructions (setArgToEnv args (reverse tempstack) env) [] of
            Right err -> Right err
            Left val -> Left (val : newstack)
    _ -> Right (s ++ " not a function")

call :: (Int, String) -> Stack -> Env -> Either Stack String
call (l, s) stack env = case isBuiltin stack (l, s) of
    (True, val) -> val
    (False, _) -> isFunc (l, s) stack env

jumpIfFalse :: [Instruction] -> Int -> Ast -> Either [Instruction] String
jumpIfFalse instructions lineJump (AstBoolean "#f") = case lineJump < 0 of
    True -> Right "if invalid jump"
    False -> removeNFromList instructions lineJump
jumpIfFalse instructions _ (AstBoolean "#t") = Left instructions
jumpIfFalse _ _ _ = Right "if invalid jump"

jumpNoCond :: [Instruction] -> Int -> Either [Instruction] String
jumpNoCond instructions lineJump = case lineJump < 0 of
    True -> Right "invalid jump"
    False -> removeNFromList instructions lineJump

-- push symbol value from env
get :: Ast -> Env -> Stack -> Either Stack String
get (AstSymbol s) env stack = case searchTupleArray env s of
    Nothing -> Right (s ++ " unknown variable")
    Just (Right var) -> Left (var:stack)
    Just (Left _) -> Right (s ++ " invalid value")
get _ _ _ = Right ("? unknown variable")

-- delete symbol from env
deleteEnv :: Ast -> Env -> Env
deleteEnv (AstSymbol s) env = removeFromTupleArray env s
deleteEnv _ env = env

-- deleteEnv 
exec :: [Instruction] -> Env -> Stack -> Either Ast String
exec (Instruction {line = _, command = "push", value = Just v}:b) env stack = case push v stack of
    (Left newstack) -> exec b env newstack
    (Right err) -> Right err
exec (Instruction {line = _, command = "get", value = Just v}:b) env stack = case get v env stack of
    (Right err) -> Right err
    (Left newstack) -> exec b env newstack
exec (Instruction {line = _, command = "deleteEnv", value = Just v}:b) env stack = exec b (deleteEnv v env) stack
exec (Instruction {line = l, command = "call", value = Just (AstSymbol s)}:b) env stack = case call (l, s) stack env of
    (Right err) -> Right err
    (Left newstack) -> exec b env newstack
exec (Instruction {line = l, command = "jumpIfFalse", value = Just (AstInteger s)}:b) env (top:stack) = case jumpIfFalse b (s - l - 1) top of
    Right err -> Right err
    Left instructions -> exec instructions env stack
exec (Instruction {line = l, command = "jump", value = Just (AstInteger s)}:b) env stack = case jumpNoCond b (s - l - 1) of
    Right err -> Right err
    Left instructions -> exec instructions env stack
exec (Instruction {line = _, command = "return", value = Nothing}:_) _ (val:_) = Left val
exec (Instruction {line = _, command = cmd, value = _}:_) _ _ = Right (cmd ++ " unknown call")
exec _ _ _ = Right ("unexpected end")
