module Solvers.Net (netSolver) where

import Solvers.Solver (GameDescription, Solution, Solver (..))

solve :: GameDescription -> Solution
solve gameDesc = "NOT IMPLEMENTED: net solver in progress"

netSolver :: Solver
netSolver = Solver {
        solver = solve
      , gameLink = "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/net.html"
    }
