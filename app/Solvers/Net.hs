module Solvers.Net where

import Data.List.Split (splitOn)
import Data.Char (digitToInt, isHexDigit)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.DisjointSet as D
import Text.Parsec
    ( char, digit, satisfy, count, many1, option, parse )
import Text.Parsec.String (Parser)

import Control.Monad (forM_, unless, when)
import Control.Monad.State

import Util.UnorderedPair (UPair(..))
import Solvers.Solver (Solver (..))

type Node = Int
type Edge = UPair Node
type Graph = S.Set Edge

newtype Cell = Cell {possibleNeighborSets :: [[Node]]}
    deriving (Eq, Show)

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
        -- these describe the graph formed by the "solved" cells
        -- components is a disjoint set of nodes, where each set represents a connected component of solved cells (i.e. cells with only one possible neighbor set)
        , components :: D.DisjointSet Node
        -- whether a cycle has been formed amongst the solved cells
        , hasCycle :: Bool
        -- this is the set of edges which have been determined to be in the solution
        , solvedGraph :: Graph 
    }
    deriving (Eq, Show)

rightOf, leftOf, aboveOf, belowOf :: (Int, Int) -> Node -> Node
rightOf (w, _) i = let col = i `mod` w in if col == w - 1 then i - (w - 1) else i + 1
leftOf (w, _) i = let col = i `mod` w in if col == 0 then i + w - 1 else i - 1
aboveOf (w, h) i = let row = i `div` w in if row == 0 then i + w * (h - 1) else i - w
belowOf (w, h) i = let row = i `div` w in if row == h - 1 then i - w * (h - 1) else i + w

cellSolved :: Game -> Node -> Bool
cellSolved g i = (1==) $ length $ possibleNeighborSets $ cells g !! i

modifyCell :: Node -> (Cell -> Cell) -> Game -> Game
modifyCell i f g = g { cells = cells' }
    where
        c = cells g !! i
        (p, s) = splitAt i (cells g)
        cells' = p ++ f c:drop 1 s

-- add an edge in the solved graph
linkCells :: Node -> Node -> State Game ()
linkCells i j = do
    g <- get
    when (S.notMember (UPair i j) (solvedGraph g)) $ do
        modify $ \g -> g { solvedGraph = S.insert (UPair i j) (solvedGraph g) }
        -- insert i and j just to be sure, this should do nothing most of the time
        let d = D.insert i $ D.insert j $ components g
        -- these patterns can't fail because i and j were just inserted
        let ~(Just ri, d') = D.representative' i d
        let ~(Just rj, d'') = D.representative' j d'

        -- if i and j are already connected, we're causing a cycle
        when (ri == rj) $ modify $ \g -> g { hasCycle = True }

        -- actually link i and j
        modify $ \g -> g { components = D.union i j d'' }

markIfSolved :: Node -> State Game ()
markIfSolved i = do
    g <- get
    when (cellSolved g i) $ do
        put $ g { components = D.insert i (components g) }
        forM_ (filter (cellSolved g) $ head $ possibleNeighborSets $ cells g !! i) $ linkCells i

excludeNeighbor :: Node -> Cell -> Cell
excludeNeighbor i (Cell ps) = Cell $ filter (notElem i) ps

includeNeighbor :: Node -> Cell -> Cell
includeNeighbor i (Cell ps) = Cell $ filter (elem i) ps

excludeEdge :: Edge -> State Game ()
excludeEdge (UPair i j) = do
    modify $ modifyCell i (excludeNeighbor j)
    modify $ modifyCell j (excludeNeighbor i)

    mapM_ markIfSolved [i, j]

includeEdge :: Edge -> State Game ()
includeEdge (UPair i j) = do
    modify $ modifyCell i (includeNeighbor j)
    modify $ modifyCell j (includeNeighbor i)

    mapM_ markIfSolved [i, j]

-- the edges of the toroidal grid graph
allPossibleEdges :: Int -> Int -> [Edge]
allPossibleEdges w h = [UPair i j
    | i <- [0..w*h-1]
    , j <- ($ (w, h)) <$> [rightOf, belowOf] <*> [i]
    ]

------------------ parsing -------------------
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

        -- order matches bitmask: bit 0=right, 1=up, 2=left, 3=down
        dirFns :: [Node -> Node]
        dirFns = ($ (w, h)) <$> [rightOf, aboveOf, leftOf, belowOf]

        removeEdge g i j = S.delete (UPair i j) g

        -- start with the full, toroidal grid graph
        toroidalGridGraph = S.fromList $ allPossibleEdges w h

        -- remove wrapping edges if necessary
        gridGraph = if wraps then toroidalGridGraph else
            let withoutTopRowEdges = foldl (\g i -> removeEdge g i (aboveOf (w, h) i)) toroidalGridGraph [0..w-1]
                withoutLeftColEdges = foldl (\g i -> removeEdge g i (leftOf (w, h) i)) withoutTopRowEdges [0,w..w*(h-1)]
            in withoutLeftColEdges

        -- remove edges according to cell barriers 
        ambGraph = foldl (\g (i, CellDesc _ hasV hasH) ->
            let g'  = if hasV then removeEdge g  i (rightOf (w, h) i) else g
                g'' = if hasH then removeEdge g' i (belowOf (w, h) i) else g'
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
        , components   = D.empty
        , hasCycle     = False
        , solvedGraph  = S.empty
        }

parseDesc :: String -> Either String Game
parseDesc gameDesc =
    let withoutURL = last $ splitOn "#" gameDesc in
    case parse gameDescParser "" withoutURL of
        Left err                       -> Left (show err)
        Right (w, h, wraps, cellDescs) -> Right (buildGame w h wraps cellDescs)


------------------- solving -------------------

-- a move consists of a node, the neighbors it should be connected to, and a "justification" string
data Move = Move Node [Node] String
    deriving (Eq, Show)
type SolverPass = State Game [Move]
type Solution = ([Move], Game)

-- check that the game state is consistent with a possible solution
-- i.e. it is not immediately breaking any rules (e.g. a cycle amongst
-- solved cells, or a component which can never connect to other cells)
-- this does not necessarily mean the game is solvable
isConsistent :: Game -> Bool
isConsistent g = and [
        -- of course, every cell should have at least one possible orientation
        all (not . null . possibleNeighborSets) $ cells g
      , noIsolatedComponents
      , noCycles
    ]
    where
        hasUnsolvedNeighbour i = any (not . cellSolved g) $ head $ possibleNeighborSets $ cells g !! i
        noIsolatedComponents = all (any hasUnsolvedNeighbour) $ D.toLists $ components g
        noCycles = not $ hasCycle g

isContradiction :: Game -> Bool
isContradiction = not . isConsistent

-- game is solved if all cells have only one possible set of neighbors
-- this assumes the game is consistent, according to isConsistent
solved :: Game -> Bool
solved = all ((1==) . length . possibleNeighborSets) . cells

-- generate a list of moves by comparing one game state to another
wholeSale :: Game -> Game -> String -> [Move]
wholeSale g1 g2 msg = [Move i (head p2) msg
        | (i, Cell p1, Cell p2) <- zip3 [0..] (cells g1) (cells g2)
        , length p1 /= 1 && length p2 == 1
    ]

composeSolverPasses :: [SolverPass] -> SolverPass
-- passes are applied in list order, e.g [p1, p2] means do pass 1 first then pass 2
-- so it is important to use foldl here
composeSolverPasses passes = do
    ms <- sequence passes
    return $ concat ms

repeatPass :: Int -> SolverPass -> SolverPass
repeatPass n = composeSolverPasses . replicate n

repeatUntilNoChange :: SolverPass -> SolverPass
repeatUntilNoChange p = do
    g <- get
    ms <- p
    g' <- get
    if solved g
    then return []
    else if g == g'
    then return ms
    else do
        ms' <- repeatUntilNoChange p
        return $ ms ++ ms'

shouldExclude :: Game -> Edge -> Bool
shouldExclude g e@(UPair i j) = or [
        S.notMember e (ambientGraph g)
        , all (notElem j) (possibleNeighborSets $ cells g !! i)
        , all (notElem i) (possibleNeighborSets $ cells g !! j)
    --   , all (all ((==1) . length)) $ map (possibleNeighborSets . (cells g !!)) [i, j]
    ]

shouldInclude :: Game -> Edge -> Bool
shouldInclude g (UPair i j) = or [
        all (elem i) (possibleNeighborSets $ cells g !! j)
        , all (elem j) (possibleNeighborSets $ cells g !! i)
    ]

-- consider each edge in turn and rule them out/in based on the local structure of
-- the graph (either the ambient graph, or the spanning tree being built)
localStructurePass :: SolverPass
localStructurePass = do
    g <- get
    let es = allPossibleEdges (width g) (height g)
    forM_ es edgewiseAction
    g' <- get
    return $ wholeSale g g' "every other orientation requires an impossible edge or excludes a required edge"
    where
        edgewiseAction e = do
            g <- get
            if shouldExclude g e
            then excludeEdge e
            else if shouldInclude g e
            then includeEdge e
            else return ()

-- this is a non-local reasoning pass
-- we go over each edge in some order (the ordering is heuristic, meant to ensure we will make a good move sooner than later)
-- and decide whether to include it or exclude it by simply trying each option
-- and seeing if either one leads to a definite answer (either a solution or a contradiction)
-- if so, we terminate and return that move 
boundedLookAheadPass :: Int -> SolverPass
boundedLookAheadPass n = do
    og <- get
    let es = candidateEdges og
    forM_ es $ tryEdge og
    g' <- get
    return $ wholeSale og g' $ "bruteforce look ahead (<=" ++ show n ++ " steps)"
    where
        candidateEdges g = filter (not . isEdgeDetermined) (allPossibleEdges (width g) (height g))
            where
                isEdgeDetermined e = shouldExclude g e || shouldInclude g e

        look = repeatPass n localStructurePass

        tryEdge og e = do
            g <- get
            -- exit early if og /= g (i.e, we have already found a move to make in an earlier iteration)
            unless (og /= g) $ do
                -- try to include e and look ahead
                includeEdge e
                _ <- look
                gIn <- get
                put g

                -- try to exclude e and look ahead
                excludeEdge e
                _ <- look
                gOut <- get
                put g

                -- it is important we check isContradiction first before checking solved
                -- since solved has "not isContradiction" as a precondition 
                if isContradiction gIn                      -- including is obv. wrong
                then excludeEdge e 
                else if solved gIn || isContradiction gOut  -- including is obv. right or excluding is obv. wrong
                then includeEdge e
                else if solved gOut                         -- excluding is obv. wrong 
                then excludeEdge e 
                else return ()                              -- neither test is conclusive, just ignore this edge

-- in the interest of not looking ahead too much, we start with a small bound
-- on the look ahead and keep doubling until we can decide a move
lookAheadPass :: Int -> SolverPass
lookAheadPass max = do
    g <- get
    if solved g
    then return []
    else go initialBound 
    where
        initialBound = 1
        go n = if n > max then return [] else do
            m <- boundedLookAheadPass n
            if not $ null m
            then return m
            else go (2 * n)


solve' :: Game -> Either String Solution
solve' g = extractSolution g' moves where
    extractSolution g' moves = Right (moves, g')
    -- extractSolution g' moves = if solved g' then Right (moves, g') else Left "Could not solve the puzzle with the implemented techniques"

    solver = repeatUntilNoChange $ composeSolverPasses [
            repeatUntilNoChange localStructurePass
          , lookAheadPass (numNodes g * 4)
        ]

    (moves, g') = runState solver g

gridPos :: Game -> Node -> (Int, Int)
gridPos g i = (i `div` w, i `mod` w) where w = width g

boxDrawing :: Game -> Node -> [Node] -> String
boxDrawing g i ns = drawings !! index where
    w = width g
    h = height g
    drawings = [
            " "   -- 0000 no neighbors
          , "□─"  -- 0001 right
          , "↑"   -- 0010 up
          , "╚"   -- 0011 right, up
          , "-□"  -- 0100 left
          , "═"   -- 0101 right, left
          , "╝"   -- 0110 up, left
          , "╩"   -- 0111 right, up, left
          , "↓"   -- 1000 down
          , "╔"   -- 1001 right, down
          , "║"   -- 1010 up, down
          , "╠"   -- 1011 right, up, down
          , "╗"   -- 1100 left, down
          , "╦"   -- 1101 right, left, down
          , "╣"   -- 1110 up, left, down
          , "╬"   -- 1111 all neighbors
        ]
    ~[r, u, l, d] = fromEnum <$> (`elem` ns) <$> ($ i) <$> ($ (w, h)) <$> [rightOf, aboveOf, leftOf, belowOf]
    index = 2^0*r + 2^1*u + 2^2*l + 2^3*d

formatMove :: Game -> Move -> String
formatMove g (Move i ns justification) =
    let (row, col) = gridPos g i
    in show (row,col) ++ ": " ++ boxDrawing g i ns ++ " (neighbors: " ++ show (map (gridPos g) ns) ++ "). REASON: " ++ justification ++ "\n"

printSolution :: Solution -> String
printSolution (moves, finalGame) =
    let moveStrs = map (formatMove finalGame) moves
        extra = if solved finalGame then ["The puzzle is solved!"] else ["Could not solve the puzzle with the implemented techniques."]
    in unlines $ moveStrs ++ extra

solve :: String -> String
solve gameDesc = let
  soln = do
    game <- parseDesc gameDesc
    result <- solve' game
    return $ printSolution result
  in case soln of
    Left err -> "Error: " ++ err
    Right moves -> moves

netSolver :: Solver
netSolver = Solver {
        solver = solve
      , gameLink = "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/net.html"
    }
