module Board (
  Board (Board),
  rows,
  size,
  Row,
  clusteredBoard,
  finished,
  count,
  makeBoard,
  emptyBoard,
  getCell,
  putVal,
  countBoard,
  revealBoard
) where

import Value
import Cell
import Data.List(nub, sort, delete)
import Control.Lens
-- import qualified Data.Vector.Unboxed.Mutable as VUM
-- import Control.Monad
-- import Control.Monad.ST
-- import Data.STRef

type Row   = [Value]
data Board = Board {size :: Int, rows :: [Row]}

-- $setup
-- >>> import Test.QuickCheck
-- >>> let board = Board {size = 2, rows = [[A, A], [S, E]]}

instance Show Board where
  show board = foldr (\row rs -> showRow row ++ "\n" ++ rs) "" (rows board)
    where
      showRow row = "|" ++ foldr (\e z -> " " ++ show e ++ " |" ++ z) "" row

-- Count occurrences.
count :: Eq a => a -> [a] -> Int
count val = length . filter (== val)

-- | Create a board of size n x n with an initial value.
makeBoard :: Int -> Value -> Board
makeBoard n v = Board {size = n, rows = replicate n $ replicate n v}

-- | Starting, hidden 10x10 board.
emptyBoard :: Board
emptyBoard = makeBoard 10 U

-- | A board with all ships clustered together.
clusteredBoard :: Board
clusteredBoard = Board {
  size = 10,
  rows = [[A, A, A, F, F, F, E, E, E, E],
          [S, A, A, A, F, C, E, E, E, E],
          [S, D, D, D, D, C, E, E, E, E],
          [B, B, B, B, B, C, E, E, E, E],
          [E, E, E, E, E, E, E, E, E, E],
          [E, E, E, E, E, E, E, E, E, E],
          [E, E, E, E, E, E, E, E, E, E],
          [E, E, E, E, E, E, E, E, E, E],
          [E, E, E, E, E, E, E, E, E, E],
          [E, E, E, E, E, E, E, E, E, E]]}

-- | Get the value in a cell, we assume valid coordinates.
getCell :: Cell -> Board -> Value
getCell (Cell i j) board = rows board !! i !! j

-- | We consider the board and coordinates to be valid.
--
-- >>> rows $ putVal X (Cell 0 0) (Board 1 [[E]])
-- [[X]]
--
-- prop> \c@(Cell i j) -> getCell c (putVal v c (makeBoard (1 + max i j) (succVal v))) == v
--
putVal :: Value -> Cell -> Board -> Board
putVal val (Cell i j) board =
  board {rows = rows board & element i . element j .~ val}

-- | Count the number of occurrences of a given value in a board.
--
-- >>> countBoard A board
-- 2
--
-- >>> countBoard S board
-- 1
--
-- >>> countBoard B board
-- 0
--
-- prop> countBoard v (Board 0 []) == 0
--
-- prop> \(Positive n) -> countBoard v (makeBoard n v) == n * n
--
-- prop> \(Positive n) -> countBoard (succVal v) (makeBoard n v) == 0
--
countBoard :: Value -> Board -> Int
countBoard val board = count val (concat $ rows board)

-- | Reveal a fully sunk boat.
--
-- >>> rows $ revealBoard A board (Board 2 [[X, X], [U, U]])
-- [[A,A],[_,_]]
--
-- prop> rows (revealBoard v (Board 2 [[v, v], [succVal v, succVal v]]) (Board 2 [[X, X], [U, U]])) == [[v, v], [U, U]]
--
revealBoard :: Value -> Board -> Board -> Board
revealBoard value (Board _ base) brd@(Board _ board) =
  brd {rows = (map . map) select (zipWith zip base board)}
  where
    select (a, b)
      | a == value = a
      | otherwise = b

-- | Tell whether a game has finished.
--
-- >>> finished board
-- False
--
-- >>> finished emptyBoard
-- False
--
-- >>> finished clusteredBoard
-- True
--
-- finished :: Board -> Bool
-- finished board = elems == boats
--   where
--     elems = sort $ filter (`elem` boats) $ nub $ concat $ rows board

finished :: Board -> Bool
finished board = loop (concat (rows board)) boats
  where
    loop _ [] = True
    loop [] _ = False
    loop (val:vals) unseenBoats =
      if val == X
      then False
      else loop vals (delete val unseenBoats)

-- finished :: Board -> Bool
-- finished board =
--   runST $ do
--     let target = length boats
--     seen <- VUM.replicate (fromEnum (maxBound :: Value) + 1) False
--     forM_ [minBound..maxBound::Value] $ \v -> do
--       when (not (v `elem` boats)) $ do
--         VUM.write seen (fromEnum v) True
--     numSeen <- newSTRef (0 :: Int)
--     let loop vals = do
--           ns <- readSTRef numSeen
--           if ns == target
--             then pure True
--             else do
--               case vals of
--                 [] -> pure False
--                 (val:rest) ->
--                   if val == X
--                   then pure False
--                   else do
--                     b <- VUM.read seen (fromEnum val)
--                     when (not b) $ do
--                       VUM.write seen (fromEnum val) True
--                       modifySTRef' numSeen (+1)
--                     loop rest
--     loop (concat (rows board))
