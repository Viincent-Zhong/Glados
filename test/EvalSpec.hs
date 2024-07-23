module EvalSpec (spec) where

import Test.Hspec
import Eval
import Lib



jumpSpec :: Spec
jumpSpec = do
    describe "jump" $ do
        it "returns an empty list when given an empty list" $ do
            jump [] 1 `shouldBe` []

        it "returns the entire list when the given line number is not found" $ do
            jump [
                Instruction {line = 1, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 2, command = "push", value = Just (AstInteger 2)}
                ] 3 `shouldBe` []

        it "returns the list from the specified line number to the end" $ do
            jump [
                    Instruction {line = 1, command = "push", value = Just (AstInteger 1)},
                    Instruction {line = 2, command = "push", value = Just (AstInteger 2)},
                    Instruction {line = 3, command = "jump", value = Just (AstInteger 1)},
                    Instruction {line = 4, command = "push", value = Just (AstInteger 3)}
                ] 3 `shouldBe` [
                    Instruction {line = 3, command = "jump", value = Just (AstInteger 1)},
                    Instruction {line = 4, command = "push", value = Just (AstInteger 3)}
                ]
        it "returns the empty list if given an empty list" $
            jump [] 1 `shouldBe` []
        it "returns the original list if the lineNum is not found" $
            jump [Instruction 1 "add" Nothing, Instruction 2 "mult" Nothing] 3 `shouldBe` []
        it "returns the list from the matched lineNum instruction onwards" $
            jump [Instruction 1 "add" Nothing, Instruction 2 "mult" Nothing, Instruction 3 "sub" Nothing] 2 `shouldBe` [Instruction 2 "mult" Nothing, Instruction 3 "sub" Nothing]




compileSpec :: Spec
compileSpec = do

    describe "exec compile" $ do
        it "(if #t 1 0)" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstBoolean "#t")},
                Instruction {line = 1, command = "jumpIfFalse", value = Just (AstInteger 4)},
                Instruction {line = 2, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 3, command = "return", value = Nothing},
                Instruction {line = 4, command = "push", value = Just (AstInteger 0)},
                Instruction {line = 5, command = "return", value = Nothing}
                ] [] [] `shouldBe` (Left (AstInteger 1))
        it "empty" $ do
            exec [] [("x", Right (AstInteger 1))] [] `shouldBe` (Right "unexpected end")

        it "(x 1 2)" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 2, command = "call", value = Just (AstSymbol "x")},
                Instruction {line = 3, command = "return", value = Nothing}
                ] [] [] `shouldBe` (Right "x undefined function")

        it "(x 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 2, command = "push", value = Just (AstInteger 3)},
                Instruction {line = 3, command = "push", value = Just (AstInteger 4)},
                Instruction {line = 4, command = "push", value = Just (AstInteger 5)},
                Instruction {line = 5, command = "push", value = Just (AstInteger 6)},
                Instruction {line = 6, command = "push", value = Just (AstInteger 7)},
                Instruction {line = 7, command = "push", value = Just (AstInteger 8)},
                Instruction {line = 8, command = "push", value = Just (AstInteger 9)},
                Instruction {line = 9, command = "push", value = Just (AstInteger 10)},
                Instruction {line = 10, command = "push", value = Just (AstInteger 11)},
                Instruction {line = 11, command = "push", value = Just (AstInteger 12)},
                Instruction {line = 12, command = "push", value = Just (AstInteger 13)},
                Instruction {line = 13, command = "push", value = Just (AstInteger 14)},
                Instruction {line = 14, command = "push", value = Just (AstInteger 15)},
                Instruction {line = 15, command = "push", value = Just (AstInteger 16)},
                Instruction {line = 16, command = "call", value = Just (AstSymbol "x")},
                Instruction {line = 17, command = "return", value = Nothing}
                ] [] [] `shouldBe` (Right "x undefined function")

        it "compile unexpected end" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 1, command = "call", value = Just (AstSymbol "lambda")},
                Instruction {line = 2, command = "deleteEnv", value = Just (AstSymbol "lambda")},
                Instruction {line = 3, command = "return", value = Nothing}
                ] [
                    ("lambda", Left (["x"],
                    [Instruction {line = 0, command = "push", value = Just (AstInteger 2)}]))
                ] [] `shouldBe` (Right "unexpected end")

        it "compile unexpected end" $ do
            exec [] [("x", Right (AstInteger 5))] [] `shouldBe` (Right "unexpected end")

        it "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1))))) (fact 10)" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 10)},
                Instruction {line = 1, command = "call", value = Just (AstSymbol "fact")},
                Instruction {line = 2, command = "return", value = Nothing}
                ] [
                    ("fact", Left (["x"],
                    [
                        Instruction {line = 0, command = "get", value = Just (AstSymbol "x")},
                        Instruction {line = 1, command = "push", value = Just (AstInteger 1)},
                        Instruction {line = 2, command = "call", value = Just (AstSymbol "eq?")},
                        Instruction {line = 3, command = "jumpIfFalse", value = Just (AstInteger 6)},
                        Instruction {line = 4, command = "push", value = Just (AstInteger 1)},
                        Instruction {line = 5, command = "return", value = Nothing},
                        Instruction {line = 6, command = "get", value = Just (AstSymbol "x")}, -- pop *
                        Instruction {line = 7, command = "get", value = Just (AstSymbol "x")}, -- pop -
                        Instruction {line = 8, command = "push", value = Just (AstInteger 1)}, -- pop -
                        Instruction {line = 9, command = "call", value = Just (AstSymbol "-")}, -- pop 2 | pop fact
                        Instruction {line = 10, command = "call", value = Just (AstSymbol "fact")}, -- pop 1 | pop *
                        Instruction {line = 11, command = "call", value = Just (AstSymbol "*")}, -- pop 2
                        Instruction {line = 12, command = "return", value = Nothing}
                    ])
                )
                ] [] `shouldBe` (Left (AstInteger 3628800))
-- exec :: [Instruction] -> Env -> Stack -> Either Ast String
        it "(define (fib-it a b n) (if (< n 1) a (fib-it b (+ a b) (- n 1)))) (define (fib n) (fib-it 0 1 n)) (fib 20)" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 20)},
                Instruction {line = 1, command = "call", value = Just (AstSymbol "fib")},
                Instruction {line = 2, command = "return", value = Nothing}
                ] [
                ("fib", Left (["n"], [
                    Instruction {line = 0, command = "push", value = Just (AstInteger 0)},
                    Instruction {line = 1, command = "push", value = Just (AstInteger 1)},
                    Instruction {line = 2, command = "get", value = Just (AstSymbol "n")},
                    Instruction {line = 3, command = "call", value = Just (AstSymbol "fib-it")},
                    Instruction {line = 4, command = "return", value = Nothing}
                ])),
                ("fib-it", Left (["a", "b", "n"], [
                    Instruction {line = 0, command = "get", value = Just (AstSymbol "n")},
                    Instruction {line = 1, command = "push", value = Just (AstInteger 1)},
                    Instruction {line = 2, command = "call", value = Just (AstSymbol "<")},
                    Instruction {line = 3, command = "jumpIfFalse", value = Just (AstInteger 6)},
                    Instruction {line = 4, command = "get", value = Just (AstSymbol "a")},
                    Instruction {line = 5, command = "return", value = Nothing},
                    Instruction {line = 6, command = "get", value = Just (AstSymbol "b")},
                    Instruction {line = 7, command = "get", value = Just (AstSymbol "a")},
                    Instruction {line = 8, command = "get", value = Just (AstSymbol "b")},
                    Instruction {line = 9, command = "call", value = Just (AstSymbol "+")},
                    Instruction {line = 10, command = "get", value = Just (AstSymbol "n")},
                    Instruction {line = 11, command = "push", value = Just (AstInteger 1)},
                    Instruction {line = 12, command = "call", value = Just (AstSymbol "-")},
                    Instruction {line = 13, command = "call", value = Just (AstSymbol "fib-it")},
                    Instruction {line = 14, command = "return", value = Nothing}
                ]))] [] `shouldBe` (Left (AstInteger 6765))
        it "compile fact" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "return", value = Nothing},
                Instruction {line = 2, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 3, command = "call", value = Just (AstSymbol "+")},
                Instruction {line = 4, command = "return", value = Nothing}
                ] [] [] `shouldBe` (Left (AstInteger 1))



builtinsSpec :: Spec
builtinsSpec = do

    describe "add function" $ do
        it "+ 1 2" $ do
            add [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger 3])
        it "+ 1 -2" $ do
            add [AstInteger (-2), AstInteger 1] `shouldBe` (Left [AstInteger (-1)])
        it "+ -1 2" $ do
            add [AstInteger 2, AstInteger (-1)] `shouldBe` (Left [AstInteger 1])
        it "+ -1 -2" $ do
            add [AstInteger (-2), AstInteger (-1)] `shouldBe` (Left [AstInteger (-3)])
        it "+ 1" $ do
            add [AstInteger (1)] `shouldBe` (Right "+ invalid function call")

    describe "minus function" $ do
        it "- 1 2" $ do
            minus [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger (-1)])
        it "- 1 -2" $ do
            minus [AstInteger (-2), AstInteger 1] `shouldBe` (Left [AstInteger 3])
        it "- -1 2" $ do
            minus [AstInteger 2, AstInteger (-1)] `shouldBe` (Left [AstInteger (-3)])
        it "- -1 -2" $ do
            minus [AstInteger (-2), AstInteger (-1)] `shouldBe` (Left [AstInteger 1])
        it "- 1" $ do
            minus [AstInteger (1)] `shouldBe` (Right "- invalid function call")

    describe "mult function" $ do
        it "* 1 2" $ do
            mult [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger 2])
        it "* 1 -2" $ do
            mult [AstInteger (-2), AstInteger 1] `shouldBe` (Left [AstInteger (-2)])
        it "* -1 2" $ do
            mult [AstInteger 2, AstInteger (-1)] `shouldBe` (Left [AstInteger (-2)])
        it "* -1 -2" $ do
            mult [AstInteger (-2), AstInteger (-1)] `shouldBe` (Left [AstInteger 2])
        it "* 1" $ do
            mult [AstInteger (1)] `shouldBe` (Right "* invalid function call")

    describe "division function" $ do
        it "/ 3 1" $ do
            division [AstInteger 1, AstInteger 3] `shouldBe` (Left [AstInteger 3])
        it "/ 1 2" $ do -- I-m not sure but rounded down
            division [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger 0])
        it "/ 3 2" $ do -- I-m not sure but rounded down
            division [AstInteger 2, AstInteger 3] `shouldBe` (Left [AstInteger 1])
        it "/ 2 3" $ do -- I-m not sure but rounded down
            division [AstInteger 3, AstInteger 2] `shouldBe` (Left [AstInteger 0])
        it "/ 8 10" $ do -- I-m not sure but rounded down
            division [AstInteger 10, AstInteger 8] `shouldBe` (Left [AstInteger 0])
        it "/ 0 2" $ do
            division [AstInteger 2, AstInteger 0] `shouldBe` (Left [AstInteger 0])
        it "/ 2 0" $ do
            division [AstInteger 0, AstInteger 2] `shouldBe` (Right "div divide by zero")
        it "/ 1" $ do
            division [AstInteger (1)] `shouldBe` (Right "div invalid function call")

    describe "modulo function" $ do
        it "% 6 4" $ do
            modulo [AstInteger 4, AstInteger 6] `shouldBe` (Left [AstInteger 2])
        it "% 0 2" $ do
            modulo [AstInteger 2, AstInteger 0] `shouldBe` (Left [AstInteger 0])
        it "% 2 0" $ do
            modulo [AstInteger 0, AstInteger 2] `shouldBe` (Right "mod divide by zero")
        it "% 1" $ do
            modulo [AstInteger (1)] `shouldBe` (Right "mod invalid function call")

    describe "inferiorto function" $ do
        it "< 6 4" $ do
            inferiorto [AstInteger 4, AstInteger 6] `shouldBe` (Left [AstBoolean "#f"])
        it "< 0 2" $ do
            inferiorto [AstInteger 2, AstInteger 0] `shouldBe` (Left [AstBoolean "#t"])
        it "< 0 0" $ do
            inferiorto [AstInteger 0, AstInteger 0] `shouldBe` (Left [AstBoolean "#f"])
        it "< 1" $ do
            inferiorto [AstInteger (1)] `shouldBe` (Right "< invalid function call")

    describe "equal function" $ do
        it "should return #t for 1 1" $ do
            equal [AstInteger 1, AstInteger 1] `shouldBe` Left [AstBoolean "#t"]
        it "should return #f for 1 2" $ do
            equal [AstInteger 1, AstInteger 2] `shouldBe` Left [AstBoolean "#f"]
        it "should return an error message for an invalid function call" $ do
            equal [AstInteger 1] `shouldBe` Right "eq? invalid function call"


execSpec :: Spec
execSpec = do
    describe "test exec" $ do
        it "push success" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "return", value = Nothing}
                ] [
                    ("sucess", Left (["x"], [])),
                    ("sucess", Right (AstInteger 2))
                ] [] `shouldBe` (Left (AstInteger 1))
        it "push error" $ do
            exec [Instruction {line = 0, command = "push", value = Just (AstSymbol "x")}] [] [] `shouldBe` (Right "Error in push")
        it "get error" $ do
            exec [Instruction {line = 0, command = "get", value = Just (AstSymbol "x")}] [] [] `shouldBe` (Right "x unknown variable")
        -- it "jumpIfFalse error" $ do
        --     exec [Instruction {line = 0, command = "jumpIfFalse", value = Just (AstInteger 2)}] [] [AstBoolean "#f"] `shouldBe` (Right "if invalid jump")
        -- it "cmd error" $ do
        --     exec [Instruction {line = 0, command = "helloworld", value = Just (AstInteger 2)}] [] [] `shouldBe` (Right "helloworld unknown call")


evalSpec :: Spec
evalSpec = do
    describe "DeleteEnv function" $ do
        it "test 1" $ do
            deleteEnv (AstSymbol "y") [("x", Right (AstInteger 1)), ("y", Right (AstInteger 2))] `shouldBe` [("x", Right (AstInteger 1))]
        it "test 2" $ do
            deleteEnv (AstSymbol "z") [("x", Right (AstInteger 1)), ("y", Right (AstInteger 2))] `shouldBe` [("x", Right (AstInteger 1)), ("y", Right (AstInteger 2))]
        it "test 3" $ do
            deleteEnv (AstSymbol "x") [("x", Right (AstInteger 1))] `shouldBe` []
    describe "stackPush function" $ do
        it "test 1" $ do
            stackPush [AstInteger 2] (AstSymbol "x") `shouldBe` [AstSymbol "x", AstInteger 2]
        it "test 2" $ do
            stackPush [] (AstInteger 1) `shouldBe` [AstInteger 1]
    describe "stackPop function" $ do
        it "test 1" $ do
            stackPop [AstInteger 1] `shouldBe` []
        it "test 2" $ do
            stackPop [AstInteger 1, AstBoolean "#f"] `shouldBe` [AstBoolean "#f"]
        it "test 3" $ do
            stackPop [] `shouldBe` []
    describe "setArgToEnv" $ do
        it "test 1" $ do
            setArgToEnv [] [AstInteger 1] [] `shouldBe` []
        it "test 2" $ do
            setArgToEnv ["helloword"] [] [] `shouldBe` []
    describe "extractNFromList" $ do
        it "test error" $ do
            extractNFromList ["helloword"] 2 `shouldBe` (Right "Not enough elements from list")
    describe "removeNFromList" $ do
        it "test error" $ do
            removeNFromList ["helloword"] 2 `shouldBe` (Right "Not enough elements from list")




spec :: Spec
spec = do
    execSpec
    jumpSpec
    compileSpec
    builtinsSpec
    evalSpec
