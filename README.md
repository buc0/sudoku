# sudoku
An inferential sudok solver

The script reads STDIN for commands.  Note that `x` and `y` values given in commands are zero-based starting from the top left of the puzzle.

The three recognized commands are:
1. Set a cell to a value: `x,y=n`
2. Note that a cell cannot be a value: `x,y!=n`
3. Read an entire 9x9 grid from the next nine STDIN lines: `e`

The first two commands were designed with the idea of using this script as a tool to assist in solving a puzzle manually.  The third command was added as a way to more easily enter an entire puzzle for testing the solver's capabilities.
