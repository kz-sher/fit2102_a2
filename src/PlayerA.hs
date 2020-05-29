module PlayerA where

import Value
import Cell
import Board
import Ship


-- | This is the function stub you have to improve. Currently it plays the first
-- empty cell it finds.
--
-- The grid has the following coordinate system:
--
-- >  (i,j)
-- > + - - + - - + - - +
-- > | 0,0 | 0,1 | 0,2 |
-- > + - - + - - + - - +
-- > | 1,0 | 1,1 | 1,2 |
-- > + - - + - - + - - +
-- > | 2,0 | 2,1 | 2,2 |
-- > + - - + - - + - - +

-- | Definition of Binary Tree
data BinTree a = Nil | Node (BinTree a) a (BinTree a)
-- | Definition of Lookup Tree using Binary Tree
-- | 1st object is the key ~~ index
-- | 2nd object is the item ~~ value
type LookupTree a = BinTree (Int, a)

-- | Convert sorted list into Lookup Tree
fromList :: [(Int,Value)] -> LookupTree Value
fromList [] = Nil
fromList ls = Node (fromList l) x (fromList r)
   where 
     m = length ls `div` 2
     x = ls !! m
     l = take m ls
     r = drop (m+1) ls

-- | Binary search for cells'value
-- | Searching for cells'value is improved to log N from N^2 where N is the size of board
searchCell :: Cell -> LookupTree Value -> Value
searchCell (Cell i j) (Node l (m,x) r) 
  | index == m = x
  | index < m = searchCell (Cell i j) l
  | otherwise = searchCell (Cell i j) r
   where 
     index = i*10+j -- index is counted from left to right and top to bottom 

-- | The function to determine which play mode is used 
play :: Board -> Cell
play b@(Board _ board)
    | board == []  = error "No free cell in the board."  
    | countBoard X b == 0 = playMode bst ships 1 -- Hunt Mode
    | otherwise = playMode bst ships 2 -- Target Mode
    where
      bst = fromList (zip [0..] (concat board)) -- Construct BST
      ships=excludeSunkShip standardShips -- exclude those ships we sunk
      
      -- | The function for excluding ships we sunk
      excludeSunkShip :: [Ship] -> [Ship]
      excludeSunkShip []=[]
      excludeSunkShip (s:ss)
        | countBoard (shipValue s) b > 0 = excludeSunkShip ss
        | otherwise = [s] ++ excludeSunkShip ss

-- | The function consists of the main implementation for choosing next potential cell
-- | mode 1 is Hunt Mode which is to find a cell which can fit the most ships when no ship is hit
-- | mode 2 is Target Mode which is to find the neighbour cell which has the highest probability when a ship is hit
playMode :: LookupTree Value -> [Ship] -> Int -> Cell
playMode bst ships modeNum= playing (Cell 0 0) (Cell 5 5)
  where
    -- | do the job from left to right, top to bottom according to given mode
    playing :: Cell -> Cell -> Cell 
    playing curr@(Cell i j) target 
        | i>9 = target -- return target cell when all cells are visited
        | j>9 = playing (Cell (i + 1) 0) target -- go to next row after last column of cell is visited
        | modeNum == 1 = if probOf curr > probOf target -- in mode 1, we compare their probability and choose the highest one
                            then playing (Cell i (j+1)) curr
                         else playing (Cell i (j+1)) target
        | modeNum == 2 && (isPotentialTarget curr) = if probOf curr > probOf target -- in mode 2, we also did the same
                                                        then playing (Cell i (j+1)) curr 
                                                     else if probOf curr == probOf target -- if they have same probability, we compare their surrounding X in order to give the best cell
                                                        then playing (Cell i (j+1)) (isHighProb curr target)
                                                     else playing (Cell i (j+1)) target
        | otherwise = playing (Cell i (j+1)) target -- go to next column of cell
        where
          -- | the function to calculate the probabilty of cell
          -- | according to their ship count (in mode 1) and their x count (in mode 2)
          probOf :: Cell -> Int
          probOf cell
            | modeNum == 1 = rotatedShipCount cell
            | otherwise    = rotatedShipXCount cell

          -- | the function to find which cell is the best if their probability is the same
          isHighProb :: Cell -> Cell -> Cell
          isHighProb cell1 cell2
            | numXOf cell1 > numXOf cell2 = cell1
            | otherwise = cell2

          -- | the function to calculate the number of X surrounding with the given cell 
          numXOf :: Cell -> Int
          numXOf cell =sum (map countX (frensOf cell++[northOf (northOf cell),
                                                       southOf (southOf cell),
                                                       westOf  (westOf cell) ,
                                                       eastOf  (eastOf cell) ]))
            where
              -- | the function for calculation
              countX :: Cell -> Int
              countX c
                | isValidCell c && searchCell c bst == X = 1 -- if X is found, mark +1
                | isValidCell c && searchCell c bst /= U = -1 -- if any other than unvisited is found, mark -1
                | otherwise = 0 -- if unvisited is found, mark remains unchanged

    -- | the function to determine whether the cell is a potential target
    -- | if cell is valid, unvisited and one of surrounded cells is X
    isPotentialTarget :: Cell -> Bool
    isPotentialTarget target = isValidCell target 
                               && searchCell target bst == U
                               &&((isValidCell (northOf target) 
                                   && searchCell (northOf target) bst == X)
                               || (isValidCell (southOf target)
                                   && searchCell (southOf target) bst == X)
                               || (isValidCell (westOf target)
                                  && searchCell (westOf target) bst == X)
                               || (isValidCell (eastOf target)
                                  && searchCell (eastOf target) bst == X))

    -- | the function to calculate how many ships can fit the given cell
    -- | rotate 6 ships by 4 type of rotations
    -- | sum all the numbers
    rotatedShipCount :: Cell -> Int
    rotatedShipCount target 
        | searchCell target bst /= U = -1 
        | otherwise = sum [isValidShips (map (offsetCell target) (shipCells (rotateShip r s))) | r<-[Rotation0,Rotation1,Rotation2,Rotation3],s<-ships]

    -- | the function to determine whether the rotated ship is valid
    isValidShips :: [Cell] -> Int 
    isValidShips [] = 1 
    isValidShips ((Cell i j):cs) 
        | not (isValidCell (Cell i j)) = 0
        | searchCell (Cell i j) bst /= U = 0
        | otherwise = isValidShips cs
    
    -- | the function to calculate how many Xs the ship cells have after rotation
    -- | rotate 6 ships by 4 rotations
    -- | consider all ship cells
    -- | find which rotation has the maximum X count
    rotatedShipXCount :: Cell -> Int
    rotatedShipXCount target 
        | searchCell target bst /= U = -1 
        | otherwise = maximum [xCount (map (offsetCell (offsetCell target (Cell (-a) (-b)))) (shipCells (rotateShip r s))) 0 | r<-[Rotation0,Rotation1,Rotation2,Rotation3],s<-ships,(Cell a b)<-shipCells s]

    -- | the function to calculate how many X a ship have on it
    xCount :: [Cell] -> Int -> Int
    xCount [] num = num
    xCount ((Cell i j):cs) num
        | not (isValidCell (Cell i j)) = 0
        | searchCell (Cell i j) bst == U = xCount cs num
        | searchCell (Cell i j) bst == X = xCount cs (num+1)
        | otherwise = 0

-- | the function to check whether a cell is valid
isValidCell :: Cell -> Bool
isValidCell (Cell i j)= not (i<0 || i>9 || j<0 || j>9) 

-- |the function to return the neighbours of given cell
frensOf :: Cell -> [Cell]
frensOf cell = [northOf cell, westOf cell, southOf cell, eastOf cell]

-- |the function to return the north neighbour of given cell
northOf :: Cell -> Cell
northOf (Cell i j)= (Cell (i-1) j)

-- |the function to return the south neighbour of given cell
southOf :: Cell -> Cell
southOf (Cell i j)= (Cell (i+1) j)

-- |the function to return the west neighbour of given cell
westOf :: Cell -> Cell
westOf (Cell i j)= (Cell i (j-1))

-- |the function to return the east neighbour of given cell
eastOf :: Cell -> Cell
eastOf (Cell i j)= (Cell i (j+1))

-- | You can provide your own starting board, just make sure it is valid.
startingBoard :: Monad m => m Board
startingBoard = do
  pure clusteredBoard

-- | This is a more advanced version of the previous function, allowing you to
-- use Monads. Use wisely.
mplay :: (Monad m) => Board -> m Cell
mplay board = return (play board)
