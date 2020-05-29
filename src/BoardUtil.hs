module BoardUtil where

import Control.Monad.Random
import Data.List
import Safe

import Board
import Cell
import Ship
import Value

-- | A board with ships placed randomly.
randomBoard :: MonadRandom m => [Ship] -> m Board
randomBoard allShips = loop (makeBoard 10 E) allShips maxTries
  where
    maxTries = 1000000
    loop board [] _tries = pure board
    loop board _ss 0 = error "randomBoard"
    loop board (s:ss) tries = do
      r <- getRandom
      i <- getRandomR (0,9)
      j <- getRandomR (0,9)
      let cells = map (offsetCell (Cell i j)) (shipCells (rotateShip r s))
          values = map (flip getCellSafe board) cells
      if all (== (Just E)) values
        then let newB = foldr (\c b -> putVal (shipValue s) c b) board cells
             in loop newB ss tries
        else loop board (s:ss) (tries-1)

serialiseBoard :: Board -> String
serialiseBoard = concatMap show . concat . rows

unserialiseBoard :: String -> Board
unserialiseBoard s =
  let n = round (sqrt (genericLength s))
  in Board { size = n
           , rows = chunk n (map (\c -> read [c]) s) }

chunk n xs =
  case splitAt n xs of
    ([], []) -> []
    (start, rest) -> start : chunk n rest


-- | Get the value in a cell, if the coordinates are valid.
getCellSafe :: Cell -> Board -> Maybe Value
getCellSafe c@(Cell i j) board =
  if i < 0 || j < 0 || i >= size board || j >= size board
  then Nothing
  else Just (getCell c board)

identifiedCells :: Board -> [(Cell, Value)]
identifiedCells board = do
  (i, row) <- zip [0..] (rows board)
  (j, val) <- zip [0..] row
  pure (Cell i j, val)

chooseRandom :: MonadRandom m => [a] -> m a
chooseRandom xs = do
  idx <- getRandomR (0, length xs - 1)
  pure (xs !! idx)

neighbours :: Cell -> [Cell]
neighbours (Cell i j) =
  [ Cell (i+1) j
  , Cell (i-1) j
  , Cell i (j+1)
  , Cell i (j-1)
  ]
