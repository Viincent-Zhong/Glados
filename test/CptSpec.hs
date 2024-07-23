module CptSpec (spec) where

import CPT.Cpt
import Test.Hspec

parseSourceCodeSucceeds :: Spec
parseSourceCodeSucceeds = do
    describe "parseSourceCodeSuccess" $ do
        it "success2" $ do
            (parse "foo") `shouldBe` (CptSymbols "foo")
        it "success3" $ do
            (CptInteger 42) `shouldBe` (parse "42")
        it "success4" $ do
            (CptLists [CptSymbols "foo",CptInteger 42]) `shouldBe` (parse "(foo 42)")
        it "success5" $ do
            (CptLists [CptLists [CptSymbols "define",CptSymbols "add",CptLists [CptSymbols "lambda",CptLists [CptSymbols "a",CptSymbols "b"],CptLists [CptSymbols "+",CptSymbols "a",CptSymbols "b"]]],CptLists [CptSymbols "add",CptInteger 3,CptInteger 4]]) `shouldBe` (parse "(define add\n  (lambda (a b)\n    (+ a b)))\n(add 3 4)")
        it "success6" $ do
            (CptLists [CptLists [CptLists [CptLists [CptInteger 1,CptLists [CptInteger 2],CptInteger 3]]]]) `shouldBe` (parse "((((1(2) 3) ) ) )")
        it "success7" $ do
            (CptLists [CptInteger 1,CptInteger 2,CptInteger 3]) `shouldBe` (parse "(1 2 3)")
        it "success8" $ do
            (CptLists [CptLists [CptSymbols "foo",CptSymbols "bar",CptSymbols "baz"],CptLists [CptInteger 1,CptInteger 2,CptInteger 3],CptLists [],CptLists [CptLists [CptLists [CptLists [CptInteger 1,CptLists [CptInteger 2],CptInteger 3]]]]]) `shouldBe` (parse "(( foo bar baz )\n(1 2 3) ()\n((((1(2) 3) ) ) )\n)")
        it "success9" $ do
            (CptLists [CptLists [CptSymbols "foo",CptSymbols "bar",CptSymbols "baz"],CptLists [CptInteger 1,CptInteger 2,CptInteger 3],CptLists [],CptLists [CptLists [CptLists [CptLists [CptInteger 1,CptLists [CptInteger 2],CptInteger 3]]]]]) `shouldBe` (parse "(( foo bar baz ) # comment 1\n(1 2 3) ()#comment 2\n((((1(2) 3) ) ) )     #comment 3\n)")

spec :: Spec
spec = do
    parseSourceCodeSucceeds
