module Solvers.Flip (flipSolver) where

import Solvers.Solver (GameDescription, Solution, Solver (..))

solve :: GameDescription -> Solution
solve gameDesc = "NOT IMPLEMENTED: flip solver in progress"

flipSolver :: Solver
flipSolver = Solver {
        solver = solve
      , gameLink = "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/flip.html"
    }