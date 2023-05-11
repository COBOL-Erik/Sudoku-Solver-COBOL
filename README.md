# Sudoku-Solver-COBOL
A sudoku solver implemented in COBOL

MDSMSDUS is a COBOL program, or (sub)module, that solves a sudoku. It is invoked in COBOL as such:
```
CALL 'MDSMSDUS' USING SUDOKU (a string containing the sudoku. In and out variable)
                      SUDOKU-LENGTH (usually 9. In variable)
                      NR-OF-GUESSES-NEEDED-BEFORE-SOLUTION (out variable)
```
```
RETURN-CODE =         0       Sudoku solved
                      100     Sudoku has no solution
                      200     No solution found
                      3000    Not enough memory
                      4000    Internal error
```