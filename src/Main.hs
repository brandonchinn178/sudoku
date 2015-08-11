module Main where

import System.Environment   (getArgs)
import System.IO            (hFlush, stdout)
import Data.Char            (isDigit, digitToInt)
import Control.Monad        (when)
import Sudoku

main = do
    arguments <- getArgs
    rows <- if null arguments
            then do
                putStrLn "Please input a puzzle to solve (Ctrl + C to exit):"
                inputPuzzle 1 []
            else do
                contents <- readFile (head arguments)
                return $ lines contents
    checkFormat rows `seq` return () -- force eval
    prettyPrint $ sudoku $ parsePuzzle rows

inputPuzzle :: Int -> [String] -> IO [String]
inputPuzzle 10 base = return base
inputPuzzle i base = do
    putStr $ "Row " ++ show i ++ ": "
    hFlush stdout
    line <- getLine
    if length line == 9
        then inputPuzzle (i+1) (base ++ [line])
        else do
            let message = if length line < 9
                            then "Invalid format: less than 9 values found"
                            else "Invalid format: more than 9 values found"
            putStrLn message
            inputPuzzle i base

parsePuzzle :: [[ Char ]] -> EmptyPuzzle
parsePuzzle = map parseRow
    where parseRow = map (\char -> if isDigit char then Just (digitToInt char) else Nothing)

-- | Check the format of the file
checkFormat :: [String] -> IO ()
checkFormat rows
    | len < 9   = error "Invalid format: less than 9 rows found"
    | len > 9   = error "Invalid format: more than 9 rows found"
    | otherwise = when (all (\row -> checkFormatLine row `seq` True) rows) $ return ()
    where len = length rows

-- | Check the format for a line
checkFormatLine :: String -> IO ()
checkFormatLine line
    | len < 9   = error $ "Invalid format: less than 9 values found: " ++ line
    | len > 9   = error $ "Invalid format: more than 9 values found: " ++ line
    | otherwise = return ()
    where len = length line

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
    puzzle `seq` return () -- force eval of puzzle before doing anything
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
