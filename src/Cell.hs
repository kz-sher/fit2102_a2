module Cell where

import Test.QuickCheck.Arbitrary

data Cell = Cell !Int !Int
  deriving (Eq, Ord)

offsetCell :: Cell -> Cell -> Cell
offsetCell (Cell i j) (Cell a b) = Cell (i+a) (j+b)

instance Show Cell where
  show (Cell i j) = "(" ++ show i ++ ", " ++ show j ++ ")"

instance Arbitrary Cell where
  arbitrary = do
    i <- arbitrarySizedNatural
    j <- arbitrarySizedNatural
    return (Cell i j)
