module MinMax where
import GameStructure
import Interaction

data Tree a = Node a [Tree a] deriving (Show, Eq)

makeTree :: Game -> Mark -> (Tree Game)
makeTree Game {size = size, rows = rows} mark = Node (Game size rows) (makeNodes (Game size rows) mark)

makeNodes :: Tree Game -> Mark -> [Tree Game]
makeNodes (Node game []) mark = Node allMoves

allMoves :: Game -> Mark -> [Game]
allMoves Game {size = size, rows = rows} mark = go (0, 0) (Game size rows) mark
    where
        go :: (Int, Int) -> Game -> Mark -> [Game]
        go (row, col) g@(Game size rows) mark
            | col == size = go (row + 1, 0) (Game size rows) mark
            | row == size = []
            | otherwise = case isBlank g (row, col) of
                True        -> [makeMove mark (row, col) g] ++ go (row, col + 1) g mark
                _           -> go (row, col + 1) g mark

