module CPT.Cpt
    (
        Cpt (..),
        parse,
        formatCpt
    )
    where

import Lib
import Data.Char (isDigit, isSpace)

-- data Cpt = CptLists [Cpt] | CptSymbols String | CptInteger Int deriving (Eq, Show)

removeSpaces :: String -> String
removeSpaces = foldl go ""
  where
    go :: String -> Char -> String
    go acc c =
      if c == ' ' && (null acc || last acc == ' ' || last acc == ')' || last acc == '(')
        then acc
        else acc ++ [c]

removeNewLines :: String -> String
removeNewLines = filter (/= '\n')

parseTree :: String -> Maybe (Cpt, String)
parseTree s = case removeSpaces s of
    "" -> Nothing
    ('(':xs) -> do
        (cpts, rest) <- parseList xs
        return (CptLists cpts, rest)
    (x:xs) | x == '-' && isDigit (head xs) -> do
        (num, rest) <- parseInteger xs
        return (CptInteger (negate num), rest)
    (x:xs) | isDigit x -> do
        (num, rest) <- parseInteger (x:xs)
        return (CptInteger num, rest)
    (x:xs) -> do
        let (symbol, rest) = parseSymbol (x:xs)
        if symbol == "" then parseTree rest else return (CptSymbols symbol, rest)


parseList :: String -> Maybe ([Cpt], String)
parseList s = case s of
    (')':xs) -> Just ([], xs)
    _ -> do
        (cpt, rest) <- parseTree s
        (cpts, rest') <- parseList rest
        return (cpt:cpts, rest')

parseSymbol :: String -> (String, String)
parseSymbol s = let (symbol, rest) = span (\c -> not (isSpace c) && c /= ')') s in (symbol, dropWhile isSpace $ rest)

parseInteger :: String -> Maybe (Int, String)
parseInteger s = case reads s of
    [(n, rest)] -> Just (n, rest)
    _ -> Nothing


parseSourceCode' :: String -> Maybe [Cpt]
parseSourceCode' s = go (removeSpaces $ removeNewLines s) []
  where
    go :: String -> [Cpt] -> Maybe [Cpt]
    go "" acc = Just acc
    go ss acc = do
        (cpt, rest) <- parseTree ss
        go rest (acc ++ [cpt])

parseSourceCode :: String -> Cpt
parseSourceCode s = flatten $ case parseSourceCode' s of
    Just cpts -> CptLists cpts
    Nothing -> CptLists []

flatten :: Cpt -> Cpt
flatten (CptLists [cpt]) = cpt
flatten cpt = cpt

countLetters :: String -> Bool
countLetters str = (length $ filter (== '(') str) == (length $ filter (== ')') str)

removeComments :: String -> String
removeComments = unlines . map (takeWhile (/= '#')) . lines

replaceEquals :: String -> String
replaceEquals [] = []
replaceEquals ('=':rest) = "define" ++ replaceEquals rest
replaceEquals (c:rest) = c : replaceEquals rest

parse :: String -> Cpt
parse s = case countLetters s of
    True  -> parseSourceCode $ (replaceEquals $ removeComments s)
    False -> CptLists []

formatCpt :: Cpt -> Cpt
formatCpt cpt@(CptLists (CptLists _: CptLists _:_)) = cpt
formatCpt cpt@(CptLists [CptLists _]) = cpt
formatCpt cpt = CptLists [cpt]