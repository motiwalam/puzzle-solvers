module Solvers.AllSolvers (solvers) where

import qualified Data.Map as Map

import Solvers.Solver (Solver)
import Solvers.Flip (flipSolver)
import Solvers.Net (netSolver)

solvers :: Map.Map String Solver
solvers = Map.fromList [
    ("flip", flipSolver)
  , ("net", netSolver)
  ]