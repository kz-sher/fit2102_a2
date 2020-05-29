import Value
import Cell
import Board
import Ship
import Data.List
import Control.Lens
import Game
import PlayerA
import BoardUtil
import Control.Monad.Random


-- calculate:: Num a => a -> a -> a
-- calculate a b = function (a+b) b
--   where 
--     function a b = a + b


average :: MonadRandom m => MonadPlus m => Int -> [m Int] -> m Int
average remaining movesArray
  -- | remaining == 0 = fmap (flip (div) (length movesArray)) (foldr ((<*>) . fmap (+)) (fmap (0 +) head) movesArray)
  | remaining == 0 = msum movesArray 
  | otherwise = average (remaining - 1) (movesArray ++ [(randomBoard standardShips) >>= (runBoard mplay)])
-- average remaining [] = getRandom
-- (randomBoard standardShips) >>= (runBoard mplay)
