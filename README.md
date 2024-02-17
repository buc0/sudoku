# sudoku
An inferential sudok solver.

This solver uses only inference from existing facts.  It does not perform any brute-force solving or engage in any consideration of hypotheticals (e.g. "if this cell were a 7, how would that affect things").  As such with its current toolbox of inferences it can solve a majority of published sudoku puzzles.  The example puzzles demonstrate a bias towards collecting puzzles that require the development of additional solving methods.

As an inferational solver the script always knows the immediate reason why it adding something as a new "fact", though the one reported may be only one of several possible reasons.  There is room for improvement in this feature, such as preferring certain reasons over others or being able to report all possible reasons.

The script reads STDIN for commands.  Note that `x` and `y` values given in commands are zero-based starting from the top left of the puzzle.

The three recognized commands are:
1. Set a cell to a value: `x,y=n`
2. Note that a cell cannot be a value: `x,y!=n`
3. Read an entire 9x9 grid from the next nine STDIN lines: `e`

The first two commands were designed with the idea of using this script as a tool to assist in solving a puzzle manually.  The third command was added as a way to more easily enter an entire puzzle for testing the solver's capabilities.

Notes about saved puzzles:
* empty puzzle (template for editing)
  * `puz0`
* can be fully solved
  * `puz1`
  * `puz2`
  * `puz3`
  * `puz4`
  * `puz5`
  * `puz6`
  * `puz7`
  * `puz8`
  * `puz9`
  * `puz10`
  * `puz11`
  * `puz_vh` ("very hard")
* cannot be solved
  * `puz_e` (may have been marked as "easy", source forgotten)
  * likely from ["Simon Tatham's Portable Puzzle Collection"](https://www.chiark.greenend.org.uk/~sgtatham/puzzles/)
    * `hard1`
    * `hard2`
    * `hard3`
    * `hard4`
