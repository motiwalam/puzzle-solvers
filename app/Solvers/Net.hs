module Solvers.Net (netSolver) where

import Solvers.Solver (Solver (..))

solve :: String -> String
solve gameDesc = "NOT IMPLEMENTED: net solver in progress"

netSolver :: Solver
netSolver = Solver {
        solver = solve
      , gameLink = "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/net.html"
    }
