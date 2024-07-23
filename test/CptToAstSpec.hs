module CptToAstSpec (spec) where

import Test.Hspec
import CptToAst

cptToAstBasicSpec :: Spec
cptToAstBasicSpec = do
  describe "CptToAst Basic" $ do
    it "case Integer" $ do
      cptToAst (CptInteger 1) `shouldBe` [AstInteger 1]
    it "case true" $ do
      cptToAst (CptSymbols "true") `shouldBe` [AstBoolean "#t"]
    it "case false" $ do
      cptToAst (CptSymbols "false") `shouldBe` [AstBoolean "#f"]
    it "case a" $ do
      cptToAst (CptSymbols "a") `shouldBe` [AstSymbol "a"]

cptToAstDefineSpec :: Spec
cptToAstDefineSpec = do
  describe "CptToAst Define" $ do
    it "case define x 1" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptSymbols "x", CptInteger 1]])
        `shouldBe` [AstDefine (Left "x") (AstInteger 1)]
    it "case define x 'hello'" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptSymbols "x", CptSymbols "hello"]])
        `shouldBe` [AstDefine (Left "x") (AstSymbol "hello")]
    it "case define x false" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptSymbols "x", CptSymbols "false"]])
        `shouldBe` [AstDefine (Left "x") (AstBoolean "#f")]
    it "case define (x) 12" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "a"], CptInteger 12]])
        `shouldBe` [AstDefine (Right ["a"]) (AstInteger 12)]
    it "case define (a) 'world'" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "a"], CptSymbols "world"]])
        `shouldBe` [AstDefine (Right ["a"]) (AstSymbol "world")]
    it "case define (a) true" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "a"], CptSymbols "true"]])
        `shouldBe` [AstDefine (Right ["a"]) (AstBoolean "#t")]
  describe "CptToAst Define with suger" $ do
    it "case x = 1" $ do
      cptToAst (CptLists[CptLists[CptSymbols "x", CptSymbols "=", CptInteger 1]])
        `shouldBe` [AstDefine (Left "x") (AstInteger 1)]
    it "case x = 'hello'" $ do
      cptToAst (CptLists[CptLists[CptSymbols "x", CptSymbols "=", CptSymbols "hello"]])
        `shouldBe` [AstDefine (Left "x") (AstSymbol "hello")]
    it "case x = #f" $ do
      cptToAst (CptLists[CptLists[CptSymbols "x", CptSymbols "=", CptSymbols "false"]])
        `shouldBe` [AstDefine (Left "x") (AstBoolean "#f")]
    it "case (a) = 12" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "a"], CptSymbols "=", CptInteger 12]])
        `shouldBe` [AstDefine (Right ["a"]) (AstInteger 12)]
    it "case (a) = 'world'" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "a"], CptSymbols "=", CptSymbols "world"]])
        `shouldBe` [AstDefine (Right ["a"]) (AstSymbol "world")]
    it "case (a) = true" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "a"], CptSymbols "=", CptSymbols "true"]])
        `shouldBe` [AstDefine (Right ["a"]) (AstBoolean "#t")]

cptToAstCallSpec :: Spec
cptToAstCallSpec = do
  describe "CptToAst Call" $ do
    it "case define (x) (1)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "x"], CptLists[CptInteger 1]]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstInteger 1])]
    it "case define (x) ('hello')" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "x"], CptLists[CptSymbols "hello"]]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstSymbol "hello"])]
    it "case define (x) (false)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "x"], CptLists[CptSymbols "false"]]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstBoolean "#f"])]
    it "case define (a b c) (+ b c)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "a", CptSymbols "b", CptSymbols "c"], CptLists[CptSymbols "+", CptSymbols "b", CptSymbols "c"]]])
        `shouldBe` [AstDefine (Right ["a", "b", "c"]) (AstCall [AstSymbol "+", AstSymbol "b", AstSymbol "c"])]
    it "case define (x a b) (eq? a b)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptLists[CptSymbols "x", CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "eq?", CptSymbols "a", CptSymbols "b"]]])
        `shouldBe` [AstDefine (Right ["x", "a", "b"]) (AstCall [AstSymbol "eq?", AstSymbol "a", AstSymbol "b"])]
    it "case (a)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "a"]])
        `shouldBe` [AstCall [AstSymbol "a"]]
    it "case (1)" $ do
      cptToAst (CptLists[CptLists[CptInteger 1]])
        `shouldBe` [AstCall [AstInteger 1]]
    it "case ('hello')" $ do
      cptToAst (CptLists[CptLists[CptSymbols "hello"]])
        `shouldBe` [AstCall [AstSymbol "hello"]]
    it "case (true)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "true"]])
        `shouldBe` [AstCall [AstBoolean "#t"]]
    it "case (a 11 22)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "a", CptInteger 11, CptInteger 12]])
        `shouldBe` [AstCall [AstSymbol "a", AstInteger 11, AstInteger 12]]
    it "case (repeat 2 'hello world')" $ do
      cptToAst (CptLists[CptLists[CptSymbols "repeat", CptInteger 2, CptSymbols "hello world"]])
        `shouldBe` [AstCall [AstSymbol "repeat", AstInteger 2, AstSymbol "hello world"]]
    it "case (concat 'hello' 'world')" $ do
      cptToAst (CptLists[CptLists[CptSymbols "concat", CptSymbols "hello", CptSymbols "world"]])
        `shouldBe` [AstCall [AstSymbol "concat", AstSymbol "hello", AstSymbol "world"]]
    it "case (or true true)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "or", CptSymbols "true", CptSymbols "true"]])
        `shouldBe` [AstCall [AstSymbol "or", AstBoolean "#t", AstBoolean "#t"]]
  describe "CptToAst Call with sugar" $ do
    it "case (x) = (1)" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "x"], CptSymbols "=", CptLists[CptInteger 1]]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstInteger 1])]
    it "case (x) = ('hello')" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "x"], CptSymbols "=", CptLists[CptSymbols "hello"]]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstSymbol "hello"])]
    it "case (x) = (false)" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "x"], CptSymbols "=", CptLists[CptSymbols "false"]]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstBoolean "#f"])]
    it "case (a b c) = (b add c)" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "a", CptSymbols "b", CptSymbols "c"], CptSymbols "=", CptLists[CptSymbols "b", CptSymbols "add", CptSymbols "c"]]])
        `shouldBe` [AstDefine (Right ["a", "b", "c"]) (AstCall [AstSymbol "+", AstSymbol "b", AstSymbol "c"])]
    it "case (x a b) = (a == b)" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "x", CptSymbols "a", CptSymbols "b"], CptSymbols "=", CptLists[CptSymbols "a", CptSymbols "==", CptSymbols "b"]]])
        `shouldBe` [AstDefine (Right ["x", "a", "b"]) (AstCall [AstSymbol "eq?", AstSymbol "a", AstSymbol "b"])]
    it "case (11 a 22)" $ do
      cptToAst (CptLists[CptLists[CptInteger 11, CptSymbols "a", CptInteger 12]])
        `shouldBe` [AstCall [AstInteger 11, AstSymbol "a", AstInteger 12]]
    it "case (true && true)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "true", CptSymbols "and", CptSymbols "true"]])
        `shouldBe` [AstCall [AstSymbol "and", AstBoolean "#t", AstBoolean "#t"]]

cptToAstLambdaSpec :: Spec
cptToAstLambdaSpec = do
  describe "CptToAst Lambda" $ do
    it "case (lambda (a) (- a))" $ do
      cptToAst (CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a"], CptLists[CptSymbols "-", CptSymbols "a"]]])
        `shouldBe` [(AstLambda ["a"] (AstCall [AstSymbol "-", AstSymbol "a"]))]
    it "case ((lambda (a b) (+ a b)) 'hello' true)" $ do
      cptToAst (CptLists[CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "+", CptSymbols "a", CptSymbols "b"]], CptSymbols "hello", CptSymbols "true"]])
        `shouldBe` [(AstCall [(AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstSymbol "hello", AstBoolean "#t"])]
    it "case define x (lambda (a b) (- a b))" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptSymbols "x", CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "-", CptSymbols "a", CptSymbols "b"]]]])
        `shouldBe` [(AstDefine (Left "x") (AstLambda ["a", "b"] (AstCall [AstSymbol "-", AstSymbol "a", AstSymbol "b"])))]
    it "case define x ((lambda (a b) (+ a b)) 1 2)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "define", CptSymbols "x", CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "+", CptSymbols "a", CptSymbols "b"]], CptInteger 1, CptInteger 2]]])
        `shouldBe` [(AstDefine (Left "x") (AstCall [(AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstInteger 1, AstInteger 2]))]
  describe "CptToAst Lambda with sugar" $ do
    it "case x = (lambda (a b) (a sub b))" $ do
      cptToAst (CptLists[CptLists[CptSymbols "x", CptSymbols "=", CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "a", CptSymbols "sub",CptSymbols "b"]]]])
        `shouldBe` [(AstDefine (Left "x") (AstLambda ["a", "b"] (AstCall [AstSymbol "-", AstSymbol "a", AstSymbol "b"])))]
    it "case x = ((lambda (a b) (a + b)) 1 2)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "x", CptSymbols "=", CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "a", CptSymbols "+", CptSymbols "b"]], CptInteger 1, CptInteger 2]]])
        `shouldBe` [(AstDefine (Left "x") (AstCall [(AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstInteger 1, AstInteger 2]))]

cptToAstGlobalSpec :: Spec
cptToAstGlobalSpec = do
  describe "CptToAst Global" $ do
    it "case (define add (lambda (a b) (+ a b))) (add 3 4)" $ do
      cptToAst (CptLists [CptLists [CptSymbols "define", CptSymbols "add", CptLists [CptSymbols "lambda", CptLists [CptSymbols "a", CptSymbols "b"], CptLists [CptSymbols "+", CptSymbols "a", CptSymbols "b"]]], CptLists [CptSymbols "add", CptInteger 3, CptInteger 4]])
        `shouldBe` [AstDefine (Left "add") (AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstCall [AstSymbol "add", AstInteger 3, AstInteger 4]]
    it "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1))))) (fact 10)" $ do
      cptToAst (CptLists [CptLists [CptSymbols "define", CptLists [CptSymbols "fact", CptSymbols "x"], CptLists [CptSymbols "if", CptLists [CptSymbols "eq?", CptSymbols "x", CptInteger 1], CptInteger 1, CptLists [CptSymbols "*", CptSymbols "x", CptLists [CptSymbols "fact", CptLists [CptSymbols "-", CptSymbols "x", CptInteger 1]]]]], CptLists [CptSymbols "fact", CptInteger 10]])
        `shouldBe` [AstDefine (Right ["fact", "x"]) (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]]), AstCall [AstSymbol "fact", AstInteger 10]]
  describe "CptToAst Global with sugar" $ do
    it "((fact x) = (if (x == 1) 1 (x * (fact (x - 1))))) (fact 10)" $ do
      cptToAst (CptLists [CptLists [CptLists [CptSymbols "fact", CptSymbols "x"], CptSymbols "=", CptLists [CptSymbols "if", CptLists [CptSymbols "x", CptSymbols "==", CptInteger 1], CptInteger 1, CptLists [CptSymbols "x", CptSymbols "*", CptLists [CptSymbols "fact", CptLists [CptSymbols "x", CptSymbols "-", CptInteger 1]]]]], CptLists [CptSymbols "fact", CptInteger 10]])
        `shouldBe` [AstDefine (Right ["fact", "x"]) (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]]), AstCall [AstSymbol "fact", AstInteger 10]]

spec :: Spec
spec = do
  cptToAstBasicSpec
  cptToAstDefineSpec
  cptToAstCallSpec
  cptToAstLambdaSpec
  cptToAstGlobalSpec