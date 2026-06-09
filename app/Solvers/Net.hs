module Solvers.Net where

import Data.List.Split (splitOn)
import Data.Char (digitToInt, isHexDigit)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.List (nub)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser)

import Util.UnorderedPair (UPair(..))
import Solvers.Solver (Solver (..))

type Node = Int
type Graph = S.Set (UPair Node)

newtype Cell = Cell {possibleNeighborSets :: [[Node]]}
    deriving (Show)

data Game = Game {
        numNodes :: Int
            -- these two should be unnecessary for solving but are needed for printing solutions
        , width :: Int
        , height :: Int
            -- this is the "grid graph", minus any barriers
            -- e.g. the graph of the orthogonal adjacency relation
            -- on the cells of the grid
        , ambientGraph :: Graph
        , cells :: [Cell]
    }
    deriving (Show)

type Solution = [[Node]]

data CellDesc = CellDesc Int Bool Bool  -- bitmask, vBarrier (right), hBarrier (below)
    deriving (Show)

cellDescParser :: Parser CellDesc
cellDescParser = do
    c <- satisfy isHexDigit
    v <- option False (True <$ char 'v')
    h <- option False (True <$ char 'h')
    return $ CellDesc (digitToInt c) v h

gameDescParser :: Parser (Int, Int, Bool, [CellDesc])
gameDescParser = do
    w <- read <$> many1 digit
    _ <- char 'x'
    h <- read <$> many1 digit
    wraps <- option False (True <$ char 'w')
    _ <- char ':'
    cellDescs <- count (w * h) cellDescParser
    return (w, h, wraps, cellDescs)

buildGame :: Int -> Int -> Bool -> [CellDesc] -> Game
buildGame w h wraps cellDescs =
    let n = w * h

        rightOf i = let col = i `mod` w in if col == w - 1 then i - (w - 1) else i + 1 
        leftOf i = let col = i `mod` w in if col == 0 then i + w - 1 else i - 1
        aboveOf i = let row = i `div` w in if row == 0 then i + w * (h - 1) else i - w 
        belowOf i = let row = i `div` w in if row == h - 1 then i - w * (h - 1) else i + w

        -- order matches bitmask: bit 0=right, 1=up, 2=left, 3=down
        dirFns :: [Node -> Node]
        dirFns = [rightOf, aboveOf, leftOf, belowOf]

        insertEdge g i j = S.insert (UPair i j) g
        removeEdge g i j = S.delete (UPair i j) g

        -- start with the full, toroidal grid graph
        toroidalGridGraph = foldl (\g i -> insertEdge (insertEdge g i (rightOf i)) i (belowOf i))
                         S.empty [0..n-1]

        -- remove wrapping edges if necessary
        gridGraph = if wraps then toroidalGridGraph else
            let withoutTopRowEdges = foldl (\g i -> removeEdge g i (aboveOf i)) toroidalGridGraph [0..w-1]
                withoutLeftColEdges = foldl (\g i -> removeEdge g i (leftOf i)) withoutTopRowEdges [0,w..w*(h-1)]
            in withoutLeftColEdges

        -- remove edges according to cell barriers 
        ambGraph = foldl (\g (i, CellDesc _ hasV hasH) ->
            let g'  = if hasV then removeEdge g  i (rightOf i) else g
                g'' = if hasH then removeEdge g' i (belowOf i) else g'
            in g'') gridGraph (zip [0..] cellDescs)

        maskToNeighbors mask i =
            [ f i
            | (bit, f) <- zip [0..3] dirFns
            , mask .&. (1 `shiftL` bit) /= 0
            ]

        -- 90° clockwise: right->down, up->right, left->up, down->left
        --  = (x >> 1) | ((x & 1) << 3)
        rotateMask mask = ((mask `shiftR` 1) .|. ((mask .&. 1) `shiftL` 3)) .&. 0xF

        buildCell i (CellDesc mask _ _) =
            let rotMasks = take 4 $ iterate rotateMask mask
            in Cell { possibleNeighborSets = nub $ map (`maskToNeighbors` i) rotMasks }

    in Game
        { numNodes     = n
        , width        = w
        , height       = h
        , ambientGraph = ambGraph
        , cells        = zipWith buildCell [0..] cellDescs
        }

parseDesc :: String -> Either String Game
parseDesc gameDesc =
    let withoutURL = last $ splitOn "#" gameDesc in
    case parse gameDescParser "" withoutURL of
        Left err                       -> Left (show err)
        Right (w, h, wraps, cellDescs) -> Right (buildGame w h wraps cellDescs)

printSolution :: Game -> Solution -> String
printSolution _ _ = undefined

solve' :: Game -> Either String Solution
solve' _ = undefined

solve :: String -> String
solve gameDesc = let
  soln = do
    game <- parseDesc gameDesc
    result <- solve' game
    return $ printSolution game result
  in case soln of
    Left err -> "Error: " ++ err
    Right moves -> show moves

netSolver :: Solver
netSolver = Solver {
        solver = solve
      , gameLink = "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/net.html"
    }
