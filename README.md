# 8-Puzzle Solver (Common Lisp)

This project contains Common Lisp code for solving the 8-puzzle using multiple search strategies.

Main solver file:
- 8-puzzle-solver.cl

Other files in this folder are experiments and demos from earlier versions.

## Features

The solver supports:
- BFS (tree search)
- DFS (with visited-state pruning and safety trial cap)
- DFS with depth bound
- BFS path search
- BFS graph search (cycle removal)
- Best-first search
- A* search

Puzzle state format:
- A flat list of 9 items
- Blank tile is symbol b
- Example: (1 2 3 4 b 5 6 7 8)

## Install (Linux)

### Ubuntu / Linux Mint / Debian

1. Update packages
- sudo apt update

2. Install SBCL
- sudo apt install -y sbcl

3. Verify installation
- sbcl --version

## Run

From this project folder:
- sbcl --script "8-puzzle-solver.cl"

You should see a startup banner that the solver is ready.

## Interactive Use (REPL)

1. Start SBCL and load the file
- sbcl --load "8-puzzle-solver.cl"

2. Run a wrapper function
- (bfs '(1 2 3 4 b 5 6 7 8) '(1 2 3 b 4 5 6 7 8))
- (dfs '(1 2 3 4 b 5 6 7 8) '(1 b 3 4 2 5 6 7 8))
- (dfs-bounded '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 b 5 6 7 8) 15)
- (bfps '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 7 5 6 b 8))
- (btgs '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 7 5 6 b 8))
- (best '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))
- (a* '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))

3. Exit SBCL
- (sb-ext:exit)

## Wrapper Reference

All wrappers accept:
- start state (flat 9-item list)
- goal state (flat 9-item list)

Wrappers:
- bfs -> returns SUCCESS or FAIL
- dfs -> returns SUCCESS or FAIL
- dfs-bounded -> returns SUCCESS, FAIL, or stopped-by-depth-limit
- bfps -> returns solution path or FAIL
- btgs -> returns solution path or NIL
- best -> returns solution path or NIL
- a* -> returns solution path or NIL

## VS Code Workflow

1. Open this folder in VS Code.
2. Open Terminal in VS Code.
3. Start REPL:
- sbcl --load "8-puzzle-solver.cl"
4. Evaluate expressions directly in terminal, or send selected code from editor to terminal.

Optional keybinding (R-style):
- Bind Ctrl+Enter to command: Terminal: Run Selected Text in Active Terminal

## Troubleshooting

### Heap exhausted / enters LDB debugger

Cause:
- Search expanded too much state space (especially DFS without strong limits).

What to do:
- Prefer A* or btgs for hard goals.
- Use dfs-bounded with a sensible limit.
- Increase SBCL heap if needed:
  -This assigns arount 4 gigabytes of ram to solve
    - sbcl --dynamic-space-size 4096 --load "8-puzzle-solver.cl"
  -This assigns arount 64 gigabytes of ram to solve
    - sbcl --dynamic-space-size 65536 --load "8-puzzle-solver.cl" 

### Stuck in SBCL debugger prompt like 0[n]

Try:
- :abort
- Then exit with (sb-ext:exit)

If needed, close the terminal tab and start a new one.

## Notes

- The solver prints trial logs while searching.
- Some hard goals may still take significant time.
- A* is usually the best default choice for performance on this puzzle.
