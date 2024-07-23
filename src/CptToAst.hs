module CptToAst (
  Cpt (..),
  Ast (..),
  printCpt,
  printAst,
  cptToAst
) where

import Lib

printCpt :: Cpt -> IO ()
printCpt cpt = putStrLn (show cpt)

printAst :: Ast -> IO ()
printAst ast = putStrLn (show ast)

cptToAst :: Cpt -> [Ast]
cptToAst (CptLists [a]) = [cptToAstLine a]
cptToAst (CptLists (a : b)) = [cptToAstLine a] ++ cptToAst (CptLists b)
-- cptToAst cpt@(CptLists (CptLists ((CptSymbols "lambda"): _) : _)) = [cptToAstLine cpt]
-- cptToAst (CptLists [CptLists a]) = [cptToAstLine (CptLists a)]
-- cptToAst (CptLists (CptLists a : b)) = [cptToAstLine (CptLists a)] ++ cptToAst (CptLists b)
cptToAst cpt = [cptToAstLine cpt]

cptToAstLine :: Cpt -> Ast
cptToAstLine (CptInteger i) = AstInteger i
cptToAstLine (CptSymbols "true") = AstBoolean "#t"
cptToAstLine (CptSymbols "false") = AstBoolean "#f"
cptToAstLine (CptSymbols s) = AstSymbol s
cptToAstLine (CptLists [CptSymbols "define", CptSymbols s, v]) = AstDefine (Left s) (cptToAstLine v)
cptToAstLine (CptLists [CptSymbols "define", CptLists keys, v]) = AstDefine (Right (cptListSymbolsToStringArray keys)) (cptToAstLine v)
cptToAstLine (CptLists [CptSymbols "lambda", CptLists args, v]) = AstLambda (cptListSymbolsToStringArray args) (cptToAstLine v)
cptToAstLine (CptLists [CptSymbols a, CptSymbols "=", v]) = AstDefine (Left a) (cptToAstLine v)
cptToAstLine (CptLists [CptSymbols a, CptSymbols "define", v]) = AstDefine (Left a) (cptToAstLine v)
cptToAstLine (CptLists [CptLists a, CptSymbols "=", v]) = AstDefine (Right (cptListSymbolsToStringArray a)) (cptToAstLine v)
cptToAstLine (CptLists [CptLists a, CptSymbols "define", v]) = AstDefine (Right (cptListSymbolsToStringArray a)) (cptToAstLine v)
cptToAstLine (CptLists [a, CptSymbols "/", v]) = AstCall [(AstSymbol "div"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "div", v]) = AstCall [(AstSymbol "div"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "*", v]) = AstCall [(AstSymbol "*"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "mul", v]) = AstCall [(AstSymbol "*"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "+", v]) = AstCall [(AstSymbol "+" ), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "add", v]) = AstCall [(AstSymbol "+" ), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "%", v]) = AstCall [(AstSymbol "mod"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "mod", v]) = AstCall [(AstSymbol "mod"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "-", v]) = AstCall [(AstSymbol "-"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "sub", v]) = AstCall [(AstSymbol "-"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "eq?", v]) = AstCall [(AstSymbol "eq?"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "==", v]) = AstCall [(AstSymbol "eq?"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "<", v]) = AstCall [(AstSymbol "<"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "||", v]) = AstCall [(AstSymbol "or"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "or", v]) = AstCall [(AstSymbol "or"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "and", v]) = AstCall [(AstSymbol "and"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists [a, CptSymbols "&&", v]) = AstCall [(AstSymbol "and"), (cptToAstLine a), (cptToAstLine v)]
cptToAstLine (CptLists l) = AstCall (cptListToAst l)

cptListSymbolsToStringArray :: [Cpt] -> [String]
cptListSymbolsToStringArray (CptSymbols a : b) = [a] ++ cptListSymbolsToStringArray b
cptListSymbolsToStringArray [] = []
cptListSymbolsToStringArray _ = []

cptListToAst :: [Cpt] -> [Ast]
cptListToAst (a : b) = [cptToAstLine a] ++ cptListToAst b
cptListToAst [] = []

-- transitionCptToAst :: [Cpt] -> [Ast]
-- transitionCptToAst [] = []
-- transitionCptToAst (a:b) = cptToAst a : transitionCptToAst b

-- startCptToAst :: Cpt -> [Ast]
-- startCptToAst (CptLists (CptLists a : b)) = transitionCptToAst (CptLists a: b)
-- startCptToAst a = [cptToAst a]

-- cptToAst :: Cpt -> Ast
-- cptToAst (CptInteger i) = AstInteger i
-- cptToAst (CptSymbols "#t") = AstBoolean "#t"
-- cptToAst (CptSymbols "#f") = AstBoolean "#f"
-- cptToAst (CptSymbols s) = AstSymbol s
-- cptToAst (CptLists [CptSymbols "define", CptSymbols s, v]) = AstDefine (Left s) (cptToAst v)
-- cptToAst (CptLists [CptSymbols "define", CptLists keys, v]) = AstDefine (Right (cptListSymbolsToStringArray keys)) (cptToAst v)
-- cptToAst (CptLists [CptSymbols "lambda", CptLists args, v]) = AstLambda (cptListSymbolsToStringArray args) (cptToAst v)
-- cptToAst (CptLists l) = AstCall (cptListToAst l)

-- cptListSymbolsToStringArray :: [Cpt] -> [String]
-- cptListSymbolsToStringArray (CptSymbols a : b) = [a] ++ cptListSymbolsToStringArray b
-- cptListSymbolsToStringArray [] = []
-- cptListSymbolsToStringArray _ = []

-- cptListToAst :: [Cpt] -> [Ast]
-- cptListToAst (a : b) = [cptToAst a] ++ cptListToAst b
-- cptListToAst [] = []