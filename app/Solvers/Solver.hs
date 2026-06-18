module Solvers.Solver where
import Control.Monad.State

data Solver = Solver {
        solver :: String -> String
      , gameLink :: String
    }


type SolverPass game move = State game [move]

identity :: SolverPass g m
identity = return []

infixr 9 -->
(-->) :: SolverPass g m -> SolverPass g m -> SolverPass g m
p1 --> p2 = do
    m1 <- p1
    m2 <- p2
    return $ m1 ++ m2

fixUntil :: (g -> g -> Bool) -> SolverPass g m -> SolverPass g m
fixUntil pred pass = do
    g <- get
    ms <- pass
    g' <- get

    if pred g g'
    then return ms
    else do
        ms' <- fixUntil pred pass
        return $ ms ++ ms'

fix :: Eq g => SolverPass g m -> SolverPass g m
fix = fixUntil (==)

iter :: Int -> SolverPass g m -> SolverPass g m
iter n = foldr (-->) identity . replicate n

run :: SolverPass g m -> g -> ([m], g)
run = runState
