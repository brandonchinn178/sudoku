module Main where

import System.Environment   (getArgs)
import System.IO            (hFlush, stdout)
import Data.Char            (isDigit, digitToInt)
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
    putStrLn $ show $ length rows
    -- force eval
    checkFormat rows `seq` let solution = sudoku $ parsePuzzle rows
                           in solution `seq` prettyPrint solution

inputPuzzle :: Int -> [String] -> IO [String]
inputPuzzle 10 base = return base
inputPuzzle i base = do
    putStr $ "Row " ++ show i ++ ": "
    hFlush stdout
    line <- getLine
    case compare (length line) 9 of
        LT -> do
            putStrLn "Invalid format: less than 9 values found"
            inputPuzzle i base
        GT -> do
            putStrLn "Invalid format: more than 9 values found"
            inputPuzzle i base
        EQ -> inputPuzzle (i+1) (base ++ [line])

parsePuzzle :: [[ Char ]] -> EmptyPuzzle
parsePuzzle = map parseRow
    where parseRow = map (\char -> if isDigit char then Just (digitToInt char) else Nothing)

-- | Check the format of the file
checkFormat :: [String] -> IO ()
checkFormat rows = case compare (length rows) 9 of
                    LT -> error "Invalid format: less than 9 rows found"
                    GT -> error "Invalid format: more than 9 rows found"
                    EQ -> (all (\row -> checkFormatLine row `seq` True) rows) `seq` return ()

-- | Check the format for a line
checkFormatLine :: String -> IO ()
checkFormatLine line = case compare (length line) 9 of
                        LT -> error $ "Invalid format: less than 9 values found: " ++ line
                        GT -> error $ "Invalid format: more than 9 values found: " ++ line
                        EQ -> return ()

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
