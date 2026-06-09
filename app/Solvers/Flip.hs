module Solvers.Flip (flipSolver) where

import Numeric (readHex, showBin)
import Data.List (dropWhileEnd)
import Data.List.Split (chunksOf, splitOn)

import Solvers.Solver (Solver (..))

import Util.LinAlg (Z2(..), Vec(..), computeCoordinates)

data Board = Board {
        width :: Int
      , height :: Int
      -- stored in row major order
      , cells :: Vec Z2
      -- moves stored in row major order, one for each cell in the board
      , moves :: [Vec Z2]
    }


parse :: String -> Either String Board
parse gameDesc = do
    let withoutURL = last $ splitOn "#" gameDesc 

    ~[dims, descs] <- expect 2 "Invalid game description, missing ':'" $ 
                        splitOn ":" withoutURL
    ~[w, h] <- expect 2 "Could not parse board dimensions" $ 
                map read (splitOn "x" dims)
    let wh = w * h

    ~[basisDesc, boardDesc] <- expect 2 "Invalid game description, missing ','" $ splitOn "," descs

    let ms = map Vec $ chunksOf wh $ bitmapFromDesc (wh * wh) basisDesc
    let cs = Vec $ bitmapFromDesc wh boardDesc

    return $ Board {
        width = w
      , height = h
      , cells = cs
      , moves = ms
    }

    where
        expect :: Int -> String -> [a] -> Either String [a]
        expect n msg xs
            | length xs == n = Right xs
            | otherwise      = Left msg

        bitmapFromDesc :: Int -> String -> [Z2]
        bitmapFromDesc expectedLength desc = take expectedLength $ concatMap charToBits desc
            where
                charToBits c = map (\b -> if b == '0' then Zero else One) bits
                    where 
                        [(n, _)] = readHex [c]
                        bits = pad $ showBin n ""
                        pad x = replicate (4 - length x) '0' ++ x


solve' :: Board -> Either String (Vec Z2)
solve' board = Vec <$> computeCoordinates (\(Vec xs) -> xs) (moves board) (cells board)

printAsMatrix :: Int -> Vec Z2 -> String
printAsMatrix w (Vec cs) = unlines $ chunksOf w (map z2ToChar cs)
    where
        z2ToChar Zero = '0'
        z2ToChar One = '1'

printSolution :: Board -> Vec Z2 -> String
printSolution board (Vec ms) = printAsMatrix (width board) (Vec ms)

solve :: String -> String
solve gameDesc =
    let soln = do
            board <- parse gameDesc
            moveVec <- solve' board
            return $ printSolution board moveVec
    in case soln of
        Left err -> "Error: " ++ err
        Right s -> s 

flipSolver :: Solver
flipSolver = Solver {
        solver = solve
      , gameLink = "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/flip.html"
    }