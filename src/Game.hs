module Game (
  run,
  runBoard,
  runBoardDebug,
  human
) where
import Board
import BoardUtil
import Cell
import Value
import Control.Monad.Random
import PlayerA as PA

-- | Shoot a cell on the current board and compare with the base board.
shoot :: Cell -> Board -> Board -> (Board, String)
shoot c@(Cell x y) current@(Board _ board) base
  | x > dimension || y > dimension || x < 0 || y < 0 =
    (current, show c ++ " out of bounds.")
  | val == U  = updateBoard c base current
  | otherwise =
    (current, show c ++ " [" ++ show val ++ "] already shot.")
  where
    dimension = length board
    val  = getCell c current

-- | Update a board by either putting a Miss '.' mark, a Hit 'X' mark, or
-- revealing a fully sunk ship.
updateBoard :: Cell -> Board -> Board -> (Board, String)
updateBoard cell@(Cell _ _) base board = case (val, damages) of
  (A, 6) -> (newBoard, "You sunk the enemy aircraft carrier!")
  (B, 5) -> (newBoard, "You sunk the enemy battleship!")
  (C, 3) -> (newBoard, "You sunk the enemy cruiser!")
  (D, 4) -> (newBoard, "You sunk the enemy destroyer!")
  (F, 4) -> (newBoard, "You sunk the enemy fuel ship!")
  (S, 2) -> (newBoard, "You sunk the enemy submarine!")
  (E, _) -> (miss cell board, "Missed shot...")
  _      -> (hit  cell board, "You hit the enemy.")
  where
    val      = getCell cell base
    damages  = 1 + count (X, val) (zip (concat (rows board)) (concat (rows base)))
    hit      = putVal X
    miss     = putVal M
    newBoard = revealBoard val base board

run :: IO ()
run = do
  board <- PA.startingBoard
  game emptyBoard board 0
  where
    game :: Board -> Board -> Integer -> IO ()
    game currentBoard targetBoard shotCount
      | finished currentBoard = putStrLn $
        "You won in " ++ show shotCount ++ " shots!"
      | otherwise = do
          let cell = PA.play currentBoard
              (newBoard, _) = shoot cell currentBoard targetBoard
          game newBoard targetBoard (shotCount + 1)

runBoard :: MonadRandom m => (Board -> m Cell) -> Board -> m Int
runBoard play board =
  game emptyBoard board 0
  where
    game currentBoard targetBoard shotCount
      | finished currentBoard = pure shotCount
      | otherwise = do
          cell <- play currentBoard
          let (newBoard, _) = shoot cell currentBoard targetBoard
          game newBoard targetBoard (shotCount + 1)

runBoardDebug :: (Board -> IO Cell) -> Board -> IO Int
runBoardDebug play board =
  game emptyBoard board 0
  where
    game currentBoard targetBoard shotCount
      | finished currentBoard = pure shotCount
      | otherwise = do
          print currentBoard
          putStrLn (serialiseBoard currentBoard)
          cell <- play currentBoard
          let (newBoard, msg) = shoot cell currentBoard targetBoard
          putStrLn $ "firing on " ++ show cell ++ ": " ++ msg
          game newBoard targetBoard (shotCount + 1)

human :: IO ()
human = do
  board <- PA.startingBoard
  hgame emptyBoard board 0
  where
    hgame :: Board -> Board -> Integer -> IO ()
    hgame currentBoard targetBoard shotCount
      | finished currentBoard = putStrLn $
        "You won in " ++ show shotCount ++ " shots!"
      | otherwise = do
          putStrLn "Input i then j (i <enter> j): "
          a <- getLine
          b <- getLine
          let pair = mapM toInt [a, b]
              (newBoard, result) = case pair of
                Just [i, j] -> shoot (Cell i j) currentBoard targetBoard
                Nothing     -> (currentBoard, "Not valid coordinates.")
          putStrLn result
          print newBoard
          hgame newBoard targetBoard (shotCount + 1)

toInt :: String -> Maybe Int
toInt = fmap fst . listToMaybe . reads
  where
    listToMaybe [] = Nothing
    listToMaybe (x: _) = Just x
