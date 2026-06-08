module Solvers.Solver where

type GameDescription = String
type Solution = String

data Solver = Solver {
        solver :: GameDescription -> Solution
      , gameLink :: String
    }
