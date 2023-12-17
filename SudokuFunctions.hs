module SudokuFunctions where

import Data.List.Split (splitOn)
import Data.List (nub)
import System.Console.ANSI
import System.Random

-- Define a type alias for the Sudoku grid
type SudokuGrid = [[Char]]

-- Function to read the Sudoku grid from a file
readSudokuGrid :: FilePath -> IO SudokuGrid
readSudokuGrid filePath = do
    content <- readFile filePath
    let rows = map (concatMap show . map (read :: String -> Int) . words) (lines content)
    return rows

-- Function to replace three numbers at random indices in each row with underscores
replaceNumbers :: SudokuGrid -> IO SudokuGrid
replaceNumbers grid = mapM replaceThreeNumbers grid
  where
    replaceThreeNumbers row = do
        indicesToHide <- generateRandomIndices (length row) 3
        return $ hideAtIndex row indicesToHide

-- Function to generate a list of n unique random indices in the range [0, maxIndex)
generateRandomIndices :: Int -> Int -> IO [Int]
generateRandomIndices maxIndex n = do
    gen <- newStdGen
    return $ take n $ nub $ randomRs (0, maxIndex - 1) gen

-- Function to replace elements at specified indices in a list with underscores
hideAtIndex :: [Char] -> [Int] -> [Char]
hideAtIndex xs indices = [if i `elem` indices then '_' else x | (x, i) <- zip xs [0..]]


-- Function to print the Sudoku grid with colored numbers
printSudokuGrid :: SudokuGrid -> IO ()
printSudokuGrid grid = do
    putStrLn "##########"
    mapM_ (putStrLn . colorizeRow) grid
    putStrLn "##########"

-- Function to add background color to a row
colorizeRow :: String -> String
colorizeRow row = concatMap colorizeChar row

-- Function to add background color to a character
colorizeChar :: Char -> String
colorizeChar c = setSGRCode [SetColor Background Dull color] ++ [c] ++ setSGRCode [Reset]
  where
    color = case c of
        '_' -> Black
        '1' -> Red
        '2' -> Green
        '3' -> Yellow
        '4' -> Blue
        '5' -> Magenta
        '6' -> Cyan
        '7' -> White
        '8' -> Black
        '9' -> Magenta 
        _   -> White


-- function to read a Sudoku grid from user input
readUserSudoku :: String -> IO SudokuGrid
readUserSudoku filepath = do
    content <- readFile filepath
    let userGrid = map (concatMap show . map (read :: String -> Int) . words) (lines content)
    return userGrid

-- Function to check if the user's Sudoku grid matches the actual Sudoku
checkUserSudokuResult :: SudokuGrid -> SudokuGrid -> String
checkUserSudokuResult userGrid answerGrid
    | userGrid == answerGrid = "Congratulations! You win!"
    | otherwise = "Oops! You lose. The Sudoku grid is incorrect."