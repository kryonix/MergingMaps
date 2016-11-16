module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String

import Data.List
import Data.Char

import MergingMaps

data Maps = Maps [[RawTile]] deriving Show

maps :: Parser Maps
maps = do
    m <- many1 mapHeader
    return $ Maps m

mapHeader = do
    p <- many1 digit <* newline
    let maps = read p :: Int
    ma <- count maps singlemap
    option () $ do {char '0'; many newline; eof}
    return $ concat ma

singlemap = do
    r <- digit <* space
    let rows = digitToInt r
    c <- digit <* newline
    let cols = digitToInt c
    erg <- count rows $ count cols (oneOf $ "-"++['A'..'Z']) <* newline
    return [erg]

main = do
    [f] <- getArgs
    input <- readFile f
    case parse maps "" input of
        Left err -> do putStrLn "Error parsing input:"
                       print err
        Right (Maps erg) -> do
            let converted = map (fmap fromRawTile) erg
            let result = map mergeTiles converted
            let printable = intercalate "\n"
                                    ["Case "++show i++"\n"++x
                                    | (x, i) <- zip (map showMaps result) [1..]]
            putStrLn printable
