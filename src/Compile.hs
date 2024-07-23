module Compile
    (
        compile
    ) where

import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error (catchIOError)

import Lib
import CptToAst
import CPT.Cpt
import Eval (exec)

getcBuiltins :: [(String, Int)]
getcBuiltins = [
    ("+", 2),
    ("-", 2),
    ("*", 2),
    ("div", 2),
    ("mod", 2),
    ("<", 2),
    ("eq?", 2),
    ("if", 3)
    ]

appendInstruction :: (Either [Instruction] String) -> [Instruction] -> Env -> ((Either [Instruction] String), Int, Env)
appendInstruction a ni env = case a of
    (Right err) -> (Right err, line (last ni), env)
    (Left li) -> (Left (li ++ ni), line (last ni) + 1, env)

astListToInstructions :: [Ast] -> Int -> Env -> ((Either [Instruction] String), Int, Env)
astListToInstructions [] i env = (Left [], i, env)
astListToInstructions (a:b) i env = case astToInstructions a i env of
    (Right err, _, nenv) -> (Right err, i, nenv)
    (Left il, index, nenv) -> case astListToInstructions b index nenv of
        (Right err, _, nnenv) -> (Right err, index, nnenv)
        (Left res, ind, nnenv) -> (Left (il ++ res), ind, nnenv)

callToInstructions :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
callToInstructions (AstCall []) i env = (Right "? invalid call", i, env)
callToInstructions (AstCall (AstSymbol name:b)) i env = case searchTupleArray getcBuiltins name of
    Just (nbr) -> case length b == nbr of
        False -> (Right (name ++ " invalid call"), i, env)
        True -> astListToInstructions b i env
    Nothing -> case searchTupleArray env name of
        Nothing -> (Right (name ++ " is undefined"), i, env)
        Just (Left (args, _)) -> case length b == length args of
            False -> (Right (name ++ " invalid call"), i, env)
            True -> astListToInstructions b i env
        Just (Right _) -> (Right (name ++ " invalid call"), i, env)
callToInstructions _ i env = (Right "? invalid call", i, env)

symbolToInstructions :: String -> Int -> Env -> ((Either [Instruction] String), Int, Env)
symbolToInstructions a i env = case searchTupleArray env a of
    Nothing -> (Right (a ++ " is undefined"), i, env)
    Just _ -> (Left [Instruction {line = i, command = "get", value = Just (AstSymbol a)}], i + 1, env)

astToInstructions :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
astToInstructions (AstLambda args v) i env = case compileExpression v 0 (setEmptyArgsToEnv env args) of
    (Right err, ni, _) -> (Right err, ni, env)
    (Left li, _, _) -> (Left [Instruction {line = i, command = "call", value = Just (AstSymbol "lambda")},
        Instruction {line = i + 1, command = "deleteEnv", value = Just (AstSymbol "lambda")}], i + 2, (env ++ [("lambda", Left (args, li))])) -- to tail
astToInstructions (AstCall (AstLambda args v:b)) i env = case astListToInstructions b i env of
    (Right err, _, _) -> (Right err, i, env)
    (Left li, ni, _) -> case astToInstructions (AstLambda args v) ni env of
        (Right err, _, _) -> (Right err, i, env)
        (Left lli, nni, nenv) -> (Left (li ++ lli), nni, nenv)
astToInstructions (AstCall (AstSymbol "if":b)) i env = ifToInstructions 0 (AstCall (AstSymbol "if":b)) i env
astToInstructions (AstCall (a:b)) i env = case callToInstructions (AstCall (a:b)) i env of
    (Right err, ni, nenv) -> (Right err, ni, nenv)
    (li, ni, nenv) -> appendInstruction li [Instruction {line = ni, command = "call", value = Just a}] nenv
astToInstructions (AstSymbol a) i env = symbolToInstructions a i env
astToInstructions (AstDefine _ _) i env = (Right "define invalid call", i, env)
astToInstructions a i env = (Left [Instruction {line = i, command = "push", value = Just a}], i + 1, env)

ifToInstructions :: Int -> Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
ifToInstructions r (AstCall (_:cond:yes:no:[])) i env = case r of
    1 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + r))}]),
        lyes, Left [Instruction {line = iy, command = "return", value = Nothing}],
        lno, Left [Instruction {line = ino, command = "return", value = Nothing}]], ino + 1, nenv)
    0 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + 1))}]),
        lyes, Left [Instruction {line = iy, command = "jump", value = Just (AstInteger ino)}], -- jump end of false
        lno], ino, nenv)
    _ -> (Right "if invalid call", i, env)
    where
        (lcond, ic, cenv) = astToInstructions cond i env -- cond 
        (lyes, iy, yenv) = astToInstructions yes (ic + 1) cenv -- cond + jump + true
        (lno, ino, nenv) = astToInstructions no (iy + 1) yenv -- cond + jump + true + return | jump + false
        ifList :: [(Either [Instruction] String)] -> (Either [Instruction] String)
        ifList [] = Left []
        ifList (a:b) = case a of
            (Left il) -> case ifList b of
                (Left ret) -> Left (il ++ ret)
                Right err -> Right err
            Right err -> Right err
ifToInstructions _ _ i env = (Right "if invalid call", i, env)

setEmptyArgsToEnv :: Env -> [String] -> Env
setEmptyArgsToEnv list [] = list
setEmptyArgsToEnv list (str:ns) = setEmptyArgsToEnv ((str, Left ([], [])) : list) ns

defineInstruction :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
-- define lambda = env
defineInstruction (AstDefine (Left s) (AstLambda args v)) i env = case
    compileExpression v 0 (setEmptyArgsToEnv (insertToTupleArray env s (Left (args, []))) args) of
        (Right err, _, nenv) -> (Right err, i, nenv)
        (Left a, _, _) -> (Left [], i, insertToTupleArray env s (Left (args, a)))
-- define var
defineInstruction (AstDefine (Left s) v) i env = case
    compileExpression v i (insertToTupleArray env s (Left ([], []))) of
        (Right err, ni, nenv) -> (Right err, ni, nenv)
        (Left li, ni, nenv) -> case exec li env [] of
            Right err -> (Right err, ni, nenv)
            Left val -> (Left [], i, insertToTupleArray nenv s (Right val))
-- invalid define func
defineInstruction (AstDefine (Right []) _) i env = (Right "define invalid call", i, env)
-- define func = env
defineInstruction (AstDefine (Right (s:b)) v) i env = case
    compileExpression v 0 (setEmptyArgsToEnv (insertToTupleArray env s (Left (b, []))) b) of
        (Right err, _, nenv) -> (Right err, i, nenv)
        (Left a, _, _) -> (Left [], i, insertToTupleArray env s (Left (b, a)))
defineInstruction _ i env = (Right "? invalid call", i, env)

compileExpression :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
compileExpression (AstDefine a v) i env = defineInstruction (AstDefine a v) i env
compileExpression (AstCall (AstSymbol "if":b)) i env = ifToInstructions 1 (AstCall (AstSymbol "if":b)) i env
compileExpression a i env = case astToInstructions a i env of
    (Right err, _, nenv) -> (Right err, i, nenv)
    (Left il, index, nenv) -> (Left (il ++
        [Instruction {line = index, command = "return", value = Nothing}]), index + 1, nenv)

filterDefines :: [Ast] -> [Ast]
filterDefines [] = []
filterDefines (AstDefine n v:b) = (AstDefine n v) : filterDefines b
filterDefines (_:b) = filterDefines b

importFile :: String -> Either Env String
importFile file = case result of
    Left _ -> Right "import invalid call"
    Right content -> case compile (filterDefines (cptToAst (formatCpt (parse content)))) 0 [] of
        (Right err, _, _) -> Right ("in imported file \"" ++ file ++ "\": " ++ err)
        (Left _, _, env) -> Left env
    where
        result = unsafePerformIO (catchIOError (Right <$> readFile file) (\e -> return $ Left $ show e))

compile :: [Ast] -> Int -> Env -> (Either [Instruction] String, Int, Env)
compile [] i env = (Left [], i, env)
compile (AstCall (AstSymbol"import":AstSymbol file:[]):b) i env = case importFile file of
    Right err -> (Right err, i, env)
    Left nenv -> compile b i (nenv ++ env)
compile (a:b) i env = case compileExpression a i env of
    (Right err, ni, nenv) -> (Right err, ni, nenv)
    (Left x, index, nenv) -> case compile b index nenv of
        (Left res, ni, nnenv) -> (Left (x ++ res), ni, nnenv)
        (Right err, ni, nnenv) -> (Right err, ni, nnenv)
