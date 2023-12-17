module Main where

import SudokuFunctions

main :: IO ()
main = do
    putStrLn "Welcome to the Sudoku Game!"
    putStrLn "-----------------------------"
    putStrLn "Instructions:"
    putStrLn "1. Copy the question below and paste it into a new text file."
    putStrLn "2. Replace underscores with the actual numbers."
    putStrLn "3. Save the file and enter its filepath when prompted."
  
    -- Read the Sudoku from a file 
    putStrLn "Loading Sudoku..."
    grid <- readSudokuGrid "sudoku.txt"
    putStrLn "Sudoku loaded successfully!"
  
    -- Create a new empty Sudoku with hidden numbers
    putStrLn "Creating a new puzzle..."
    questionGrid <- replaceNumbers grid
    putStrLn "Puzzle created successfully!"
  
    -- Display the Sudoku to be filled
    putStrLn "Here is your Sudoku puzzle:\n"
    printSudokuGrid questionGrid
  
    putStrLn "\nHint: Different colors indicate that there should be no same colored number in one row.\n"
  
    -- Asking user to put answer
    putStrLn "\nEnter the filepath to your answer file (e.g., \"myAnswer.txt\"):"
  
    answerFilepath <- getLine
  
    -- Read user's answer from the provided filepath
    userAnswerGrid <- readUserSudoku answerFilepath
  
    putStrLn "\nYou entered the following Sudoku grid:"
    printSudokuGrid userAnswerGrid
  
    -- Check the user's answer and display the result
    let resultMessage = checkUserSudokuResult userAnswerGrid grid
    putStrLn resultMessage
    
    -- Prompting if user wants to play again
    putStrLn "Do you want to play again? (Enter 'r' to replay, any other character to quit):"
    replayChoice <- getLine
  
    if replayChoice == "r" || replayChoice == "R"
        then main  -- Replay the game
        else putStrLn "Thanks for playing! Goodbye!"
