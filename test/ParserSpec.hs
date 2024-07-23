module ParserSpec (spec) where

import CPT.Parser
--import CPT.Cpt
import Test.Hspec

parseCharSpec :: Spec
parseCharSpec = do
    describe "parseChar" $ do
        it "parseCharFails" $ do
            Nothing `shouldBe` (runParser (parseChar 'a') "b")
        it "parseCharSucceeds" $ do
            (Just ('a', "b")) `shouldBe` (runParser (parseChar 'a') "ab")

parseAnyCharSpec :: Spec
parseAnyCharSpec = do
    describe "parseAnyChar" $ do
        it "parserAnyCharFails" $ do
            Nothing `shouldBe` (runParser (parseAnyChar "abc") "d")
        it "parserAnyCharSucceeds" $ do
            (Just ('a', "bc")) `shouldBe` (runParser (parseAnyChar "abc") "abc")

parseOrSpec :: Spec
parseOrSpec = do
    describe "parseOr" $ do
        it "parseOrSucceeds" $ do
            (Just ('a', "bcd")) `shouldBe` (runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd")
        it "parseOrFails" $ do
            Nothing `shouldBe` (runParser (parseOr (parseChar 'a') (parseChar 'b')) "cde")
        it "parseOrSucceeds2" $ do
            (Just ('b', "cda")) `shouldBe` (runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda")

parseAndSpec :: Spec
parseAndSpec = do
    describe "parseAnd" $ do
        it "parseAndSucceeds" $ do
            (Just (('a', 'b'), "cd")) `shouldBe` (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd")
        it "parseAndSuccess2" $ do
            Nothing `shouldBe` (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda")
        it "parseAndSuccess3" $ do
            Nothing `shouldBe` (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd")
        it "parseAndFails" $ do
            Nothing `shouldBe` (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "cde")

parseAndWithSpec :: Spec
parseAndWithSpec = do
    describe "parseAndWith" $ do
        it "parseAndWithSucceeds" $ do
            (Just ("ab", "cd")) `shouldBe` (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abcd")
        it "parseAndWithSucceeds2" $ do
            Nothing `shouldBe` (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "bcda")
        it "parseAndWithSucceeds3" $ do
            Nothing `shouldBe` (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "acd")
        it "parseAndWithFail" $ do
            Nothing `shouldBe` (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "cde")

parseManySpec :: Spec
parseManySpec = do
    describe "parseMany" $ do
        it "parseManySucceeds" $ do
            (Just ("     ", "foobar")) `shouldBe` (runParser (parseMany (parseChar ' ')) "     foobar")
        it "parseManySucceeds2" $ do
            (Just ("", "foobar    ")) `shouldBe` (runParser (parseMany (parseChar ' ')) "foobar    ")
        it "parseManySucceeds3" $ do
            (Just ("", "bcd")) `shouldBe` (runParser (parseMany (parseChar 'a')) "bcd")
        it "parseManyFails" $ do
            (Just ("", "bcd")) `shouldBe` (runParser (parseMany (parseChar 'a')) "bcd")

parseSomeSpec :: Spec
parseSomeSpec = do
    describe "parseSome" $ do
        it "parseSomeSucceeds" $ do
            (Just ("42", "foobar")) `shouldBe` (runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar")
        it "parseSomeSucceeds2" $ do
            Nothing `shouldBe` (runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42")
        it "parseSomeSucceeds3" $ do
            Nothing `shouldBe` (runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar")

parseUIntSpec :: Spec
parseUIntSpec = do
    describe "parseUInt" $ do
        it "parseUIntSucceeds" $ do
            (Just (42, "foobar")) `shouldBe` (runParser parseUInt "42foobar")
        it "parseUIntSucceeds2" $ do
            Nothing `shouldBe` (runParser parseUInt "foobar42")
        it "parseUIntSucceeds3" $ do
            Nothing `shouldBe` (runParser parseUInt "foobar")
        it "parseUIntSucceeds4" $ do
            Nothing `shouldBe` (runParser parseUInt "-2")

parseIntSpec :: Spec
parseIntSpec = do
    describe "parseInt" $ do
        it "parseIntSucceeds" $ do
            (Just (42, "foobar")) `shouldBe` (runParser parseInt "42foobar")
        it "parseIntSucceeds2" $ do
            (Just (-42, "foobar")) `shouldBe` (runParser parseInt "-42foobar")
        it "parseIntSucceeds3" $ do
            Nothing `shouldBe` (runParser parseInt "foobar42")
        it "parseIntSucceeds4" $ do
            Nothing `shouldBe` (runParser parseInt "foobar")
        it "parseIntSucceeds5" $ do
            (Just (42, "")) `shouldBe` (runParser parseInt "+42")
        it "parseIntSucceeds6" $ do
            (Just (42, "foobar")) `shouldBe` (runParser parseInt "+42foobar")
        it "parseIntSucceeds7" $ do
            (Just (-42, "foobar")) `shouldBe` (runParser parseInt "-42foobar")

parsePairSpec :: Spec
parsePairSpec = do
    describe "parsePair" $ do
        it "parsePairSucceeds" $ do
            (Just ((123, 456), "foo bar")) `shouldBe` (runParser (parsePair parseInt) "(123 456)foo bar")
        it "parsePairSucceeds2" $ do
            Nothing `shouldBe` (runParser (parsePair parseInt) "(123 456foo bar")
        it "parsePairSucceeds3" $ do
            (Just ((123,456),"foo bar")) `shouldBe` (runParser (parsePair parseInt) "(123 456)foo bar")

parseListSpec :: Spec
parseListSpec = do
    describe "parseList" $ do
        it "parseListSucceeds" $ do
            (Just ([1,2,3,4,5],"foo bar")) `shouldBe` (runParser (parseList parseInt) "(1 2 3 4 5)foo bar")
        it "parseListSucceeds2" $ do
            Nothing `shouldBe` (runParser (parseList parseInt) "foo bar")
        it "parseListSucceeds3" $ do
            (Just ([1,2,3,4,5],"")) `shouldBe` (runParser (parseList parseInt) "(1 2 3 4 5)")
        it "parseListSucceeds4" $ do
            Nothing `shouldBe` (runParser (parseList parseInt) "(1 2 3 4 bonjour 5)")

spec :: Spec
spec = do
    parseCharSpec
    parseAnyCharSpec
    parseOrSpec
    parseAndSpec
    parseAndWithSpec
    parseManySpec
    parseSomeSpec
    parseUIntSpec
    parseIntSpec
    parsePairSpec
    parseListSpec