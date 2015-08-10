module Main where

import System.Environment
import Data.Char   (isDigit, digitToInt)
import Sudoku

main = do
    arguments <- getArgs
    parseArgs arguments

parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn "Please provide the filename"
parseArgs (file:_) = do
    contents <- readFile file
    let rows = lines contents
    checkFormat rows
    let solution = sudoku $ parsePuzzle rows
    -- force eval
    solution `seq` prettyPrint solution

parsePuzzle :: [[ Char ]] -> EmptyPuzzle
parsePuzzle = map parseRow
    where parseRow = map (\char -> if isDigit char then Just (digitToInt char) else Nothing)

-- |Check the format of the file
checkFormat :: [String] -> IO ()
checkFormat rows
    | length rows < 9 = error "Invalid format: less than 9 rows found"
    | length rows > 9 = error "Invalid format: more than 9 rows found"
    | any (\row -> length row < 9) rows = error "Invalid format: less than 9 values found in a row"
    | any (\row -> length row > 9) rows = error "Invalid format: more than 9 values found in a row"
    | otherwise = return ()

{-
    Pretty prints the solution in the following format:
            +-------+-------+-------+
            | 1 2 3 | 1 2 3 | 1 2 3 |
            | 1 2 3 | 1 2 3 | 1 2 3 |
            | 1 2 3 | 1 2 3 | 1 2 3 |
            +-------+-------+-------+
            | 1 2 3 | 1 2 3 | 1 2 3 |
            | 1 2 3 | 1 2 3 | 1 2 3 |
            | 1 2 3 | 1 2 3 | 1 2 3 |
            +-------+-------+-------+
            | 1 2 3 | 1 2 3 | 1 2 3 |
            | 1 2 3 | 1 2 3 | 1 2 3 |
            | 1 2 3 | 1 2 3 | 1 2 3 |
            +-------+-------+-------+
-}
prettyPrint :: Puzzle -> IO ()
prettyPrint puzzle = do
    let (a,b,c) = split3 puzzle
    printBorder
    printRows a
    printBorder
    printRows b
    printBorder
    printRows c
    printBorder
    where   border = "+-------+-------+-------+"
            printBorder = putStrLn border
            printRows = putStr . unlines . map (unwords . sepRow . (map show))
            sep = ["|"]
            sepRow row = let (a,b,c) = split3 row
                         in sep ++ a ++ sep ++ b ++ sep ++ c ++ sep
            split3 list = let (a,rest) = splitAt 3 list
                              (b,c)    = splitAt 3 rest
                          in (a,b,c)
