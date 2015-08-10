module Sudoku where

import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe)
import Data.List  ((\\), transpose)

type SudokuVal      = Int                   -- The type representing Sudoku values
type EmptyPuzzle    = [[ Maybe SudokuVal ]] -- A matrix where Nothing values represent unknowns.
                                            --    The matrix is a list of rows, where each row is
                                            --    a list of values.
type Puzzle         = [[ SudokuVal ]]       -- A matrix of final Int values. Stored as a list of
                                            --    rows, where each row is a list of values.

-- | Solves a sudoku puzzle, receiving a matrix of Maybe values and returning a
--   matrix of final values
sudoku :: EmptyPuzzle -> Puzzle
sudoku puzzle
    | isJust solvedPuzzle = map (map fromJust) $ fromJust solvedPuzzle
    | otherwise           = error "This puzzle has no solution!"
    where   solvedPuzzle = populatePuzzle (0, 0) puzzle

-- | Fills the given coordinate, with a valid number for the spot. If there are no
--   valid possibilities, return Nothing
populatePuzzle :: (Int, Int) -> EmptyPuzzle -> Maybe EmptyPuzzle
-- base case on last square in the puzzle
populatePuzzle (8,8) puzzle
    | isJust elem   = setAsJust elem
    | null possible = Nothing
    | otherwise     = setAsJust (Just $ head possible)
    where   elem       = getElem puzzle (8,8)
            possible   = getPossible puzzle (8,8)
            setAsJust  = Just . setElem puzzle (8,8)
-- recursive case for all other squares
populatePuzzle (i,j) puzzle
    | isJust elem   = setAndCall elem
    | null possible = Nothing
    | otherwise     = listToMaybe nextPuzzles
    where   elem        = getElem puzzle (i,j)
            possible    = getPossible puzzle (i,j)
            setAndCall  = populatePuzzle (nextCoord (i,j)) . setElem puzzle (i,j)
            nextPuzzles = filter isJust [ setAndCall $ Just elem | elem <- possible ]

-- | Gets all possible values for the given coordinate
getPossible :: EmptyPuzzle -> (Int, Int) -> [Int]
getPossible puzzle (i,j) = (([1..9] \\ getRowVals) \\ getColVals) \\ getSquareVals
    where   getRowVals    = catMaybes $ getRow puzzle i
            getColVals    = catMaybes $ getCol puzzle j
            getSquareVals = catMaybes $ getSquare puzzle (i,j)

------------------------
----- PUZZLE UTILS -----

-- | Gets the row at the given index in the matrix
getRow :: [[a]] -> Int -> [a]
getRow matrix i = matrix !! i

-- | Gets the col at the given index in the matrix
getCol :: [[a]] -> Int -> [a]
getCol matrix j = (transpose matrix) !! j

-- | Gets the square that contains the given index in the matrix
getSquare :: [[a]] -> (Int, Int) -> [a]
getSquare matrix (i,j) = [getElem matrix (x,y) | x <- rows, y <- cols]
    where   rows     = [ (getStart i) .. (getStart i)+2 ]
            cols     = [ (getStart j) .. (getStart j)+2 ]
            getStart val = 3 * (val `quot` 3)

-- | Gets the element at the given coordinate in the matrix
getElem :: [[a]] -> (Int, Int) -> a
getElem matrix (i,j) = (getRow matrix i) !! j

-- | Sets the element at the given coordinate in the matrix
setElem :: [[a]] -> (Int, Int) -> a -> [[a]]
setElem matrix (i,j) val = let (prevRows, row:nextRows) = splitAt i matrix
                               (prevCols, _:nextCols)   = splitAt j row
                           in prevRows ++ (prevCols ++ val : nextCols) : nextRows

-- | Gets the next coordinate to the given coordinate
nextCoord :: (Int, Int) -> (Int, Int)
nextCoord (i,8) = (i+1, 0)
nextCoord (i,j) = (i, j+1)
