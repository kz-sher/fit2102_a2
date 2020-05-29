import Test.QuickCheck
import Control.Monad.Random

import Game
import Board
import BoardUtil
import Ship
import Cell
import Value
import PlayerA

-- | Turn all empty cells into unknown and hide the battleship.
hide :: Board -> Board
hide (Board size rows) = Board size ((map . map) hiding rows)
  where
    hiding c = case c of
      E -> U
      B -> U
      x -> x

test_bounds :: (MonadRandom m) => m Board -> m Bool
test_bounds rand_board = do
  board <- hide <$> rand_board
  (Cell x y) <- mplay board
  return $ x <= (size board) && y <= (size board)

run_bounds :: (MonadRandom m) => m Bool
run_bounds = do
  runs <- sequence $ replicate 10 (test_bounds (randomBoard standardShips))
  return $ and runs

main :: IO ()
main = run
