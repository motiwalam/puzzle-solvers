module Main where

import Options.Applicative
import qualified Data.Map as Map

import Solvers.Solver (Solver(..))
import Solvers.AllSolvers (solvers)

data Args = Args
    { game     :: String
    , gameDesc :: String
    }

supportedList :: String
supportedList = unlines $ "Supported games:" : games where
    games = map (\(k, s) -> "  " ++ k ++ ": " ++ gameLink s) (Map.toList solvers)

argsParser :: Parser Args
argsParser = Args
    <$> argument str
          (  metavar "GAME"
          <> help "the name of the game being solved (see --supported for all valid values)"
          )
    <*> argument str
          (  metavar "GAME_DESC"
          <> help "what is called the game ID on Simon Tatham's website"
          )

opts :: ParserInfo Args
opts = info
    (argsParser
        <**> infoOption supportedList (long "supported" <> help "list supported games and exit")
        <**> helper)
    (  fullDesc
    <> progDesc "a solver for puzzles from Simon Tatham's portable puzzle collection"
    )

main :: IO ()
main = do
    args <- execParser opts
    case Map.lookup (game args) solvers of
        Nothing -> putStrLn $
            "Unknown game '" ++ game args ++ "'. Run with --supported to see available games."
        Just s  -> putStrLn $ solver s (gameDesc args)
