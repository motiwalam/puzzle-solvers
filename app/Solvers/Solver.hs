module Solvers.Solver where

data Solver = Solver {
        solver :: String -> String
      , gameLink :: String
    }
