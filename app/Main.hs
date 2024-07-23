module Main (main) where

import System.Environment (getArgs)
import CPT.Cpt
import CptToAst
import Compile
import Eval
import Prompt

-- debug :: IO ()
-- debug = do
    -- args <- getArgs
    -- content <- readFile (head args)
-- 
    -- let parsed = formatCpt (parse content)
    -- let ast = (cptToAst (parsed))
    -- let instructions = compile ast 0 []
-- 
    -- case instructions of
        -- (Right err, _, _) -> print err
        -- (Left li, _, nenv) -> case exec li nenv [] of
            -- Right err -> putStrLn err
            -- Left value -> putStrLn (show value)
-- 
    -- print (parsed)
    -- print (ast)
    -- print (instructions)

normal :: IO ()
normal = getArgs >>= \args -> case length args == 0 of
    True -> prompt []
    False -> readFile (head args) >>= \content -> case compile (cptToAst (formatCpt (parse content))) 0 [] of
        (Right err, _, _) -> print err
        (Left li, _, nenv) -> case exec li nenv [] of
            Right err -> putStrLn err
            Left value -> putStrLn (show value)

main :: IO ()
main = normal
-- main = debug