module Main where

import Control.Monad
import System.Environment
import System.Random.Shuffle

import Board
import BoardUtil
import Game
import PlayerA
import Ship

main :: IO ()
main = do
  args <- getArgs
  case (safeTwo args) of
    (Just "generateBoards", _) -> generateBoards
    (Just "runBoards", player) -> runBoards mplay
    (Just "debug", _) -> debug
    _ -> run
  where
    safeTwo l = case l of
      x: y: _ -> (Just x, Just y)
      x: [] -> (Just x, Nothing)
      [] -> (Nothing, Nothing)

generateBoards = do
  boards <- replicateM 10000 (randomBoard standardShips)
  writeFile "random-boards.txt" (unlines (map serialiseBoard boards))

runBoards player = do
  allBoards <- map unserialiseBoard . lines <$> readFile "random-boards.txt"
  boards <- shuffleM allBoards
  let f (total, n) b = do
        shots <- runBoard player b
        let total' = total + shots
            n' = n + 1
        print (n', fromIntegral total' / fromIntegral n')
        pure (total', n')
  foldM f (0, 0) (take 100 boards)
  pure ()

debug = do
  board <- head . map unserialiseBoard . lines <$> readFile "random-boards.txt"
  runBoardDebug mplay board
  pure ()
