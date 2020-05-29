module Ship (
  Ship,
  shipCells,
  shipValue,
  standardShips,
  Rotation(..),
  rotateShip
  ) where

import Cell
import Value
import System.Random

data Ship = Ship { shipValue :: Value
                 , shipCells :: [Cell] }

standardShips = [
    Ship A (f [(0,0), (0,1), (0,2),
                      (1,1), (1,2), (1,3)])
  , Ship B (f [(0,0), (0,1), (0,2), (0,3), (0,4)])
  , Ship C (f [(0,0), (0,1), (0,2)])
  , Ship D (f [(0,0), (0,1), (0,2), (0,3)])
  , Ship F (f [(0,0), (0,1), (0,2),
                      (1,1)])
  , Ship S (f [(0,0), (0,1)])
  ]
  where f = map (uncurry Cell)

data Rotation =
    Rotation0
  | Rotation1
  | Rotation2
  | Rotation3
  deriving (Enum, Bounded, Show)

instance Random Rotation where
  randomR (lo,hi) g =
    let (rint, g') = randomR (fromEnum lo, fromEnum hi) g
    in (toEnum rint, g')
  random g = randomR (minBound, maxBound) g

rotateShip :: Rotation -> Ship -> Ship
rotateShip r s =
  s { shipCells = map (rotateCell r) (shipCells s) }

rotateCell :: Rotation -> Cell -> Cell
rotateCell Rotation0 (Cell i j) = Cell i j
rotateCell Rotation1 (Cell i j) = Cell (-j) i
rotateCell Rotation2 (Cell i j) = Cell (-i) (-j)
rotateCell Rotation3 (Cell i j) = Cell j (-i)


