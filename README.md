# Haskell Chess Game

This is a chess game created in Haskell. It allows two players to play chess on the console.

## Game Features

- Player vs Player: Two human players can play against each other.
- Basic Move Suggestions: The game provides basic move suggestions for each piece.
- Console Interface: The game is played on the console using algebraic notation.

## Prerequisites

To run the chess game, you need to have Haskell installed on your system.

## How to Run

1. Clone the repository to your local machine.
2. Open a terminal and navigate to the project directory.
3. Compile the Haskell code by using an application like WinHugs and loading in the file
4. start with ```putStrLn(visualizeBoard setBoard)``` to visualize the board

## Rules and Notation

The chess game follows the standard rules of chess. Moves are entered using algebraic notation, where the columns are labeled from 'a' to 'h' and the rows are labeled from 1 to 8. For example, a typical move would be "e2 e4" to move a pawn from square e2 to e4.

## Game Interface

The game interface is text-based and displayed on the console. The board is visualized using ASCII characters. The current player and move suggestions are shown on the screen.
 moving from e2 to e4 as the first move for example: ``` move (P ('e', 2)) ('e', 4) setBoard ```
if you want to visualize the moves use this command : ``` putStrLn(visualizeBoard  (move (P ('e', 2)) ('e', 4) setBoard ))```
## Future Enhancements

- AI Opponent: Implement an AI opponent for single-player mode.
- Graphical Interface: Develop a graphical interface for a more interactive gaming experience.
- Game Saving: Allow players to save and resume games without updating the text file each time.

## Contributing

Contributions to this project are welcome. If you find any issues or have ideas for improvements, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more information.
