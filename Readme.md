# Sudoku Game in Haskell

Welcome to the Sudoku Game implemented in Haskell! This was an assignment form my Functional Programming class.
This console-based game allows you to solve Sudoku puzzles with a twist. Here's a brief guide on how to use and enjoy the game.

## How to Play

1. **Load the Sudoku Puzzle:**
    - The game starts by loading a Sudoku puzzle from the provided file (`sudoku.txt`).
    - The puzzle will be displayed with some numbers hidden, creating a challenge for the player.

2. **View the Puzzle:**
    - The Sudoku puzzle will be displayed with colored backgrounds to indicate constraints on numbers in each row.
    - Underscores (_) represent hidden numbers.

3. **User Input:**
    - Copy the displayed puzzle to a new text file.
    - Replace underscores with your answers and save the file.

4. **Submit Your Answer:**
    - Enter the file path of your answer file (e.g., "myAnswer.txt") when prompted.
    - The program will read your answer and display it.

5. **Check Your Answer:**
    - The program will check if your answer matches the actual solution.
    - If correct, you win! If not, you lose.

6. **Replay or Quit:**
    - After each round, you can choose to play again by entering 'r' or 'R'.
    - Enter any other character to quit the game.

## Implementation Details

- The Sudoku grid is loaded from a file, and some numbers are randomly hidden to create a puzzle.
- Background colors are used to visually distinguish different numbers in each row.
- Users input their answers by creating a text file with their solution.

## Run the Game

1. Ensure you have [Haskell](https://www.haskell.org/) installed on your system.

2. Clone the repository:

    ```bash
    git clone https://github.com/asilbek-0311/Simple_Sudok_FFPassignmnet.git
    cd sudoku-haskell
    ```

3. Run the game:

    ```bash
    ghci Main.hs
    main
    ```

## Personal Reflection

In this project, I applied functional programming concepts, utilized Haskell's type system, and tackled challenges such as user input and visual representation. While the game offers an engaging experience, there is room for improvement in user interaction.

Feel free to provide feedback or contribute to enhancing the game!

Enjoy playing Sudoku in Haskell!
