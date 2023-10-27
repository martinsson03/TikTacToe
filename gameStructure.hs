module GameStructure where

import Data.Char
import Data.List

type Pos = Int
data Mark = Cross | Naught | Blank deriving Eq
type Row = [Mark]
data Game = Game {size :: Int, rows :: [Row]}

instance Show Mark where
    show mark = case mark of
        Cross   -> "X"
        Naught  -> "O"
        Blank   -> "_"


instance Show Game where
    show (Game _ rows) = unlines [unwords [show mark | mark <- row]| row <- rows]

gameSize :: Game -> (Int, Int)
gameSize (Game _ rows) = (length rows, length $ rows !! 0)

createGame :: Int -> Game
createGame size = Game size (replicate size $ replicate size Blank)

testGame :: Game
testGame = Game 3 [[Naught, Naught, Cross], [Blank, Cross, Naught], [Blank, Cross, Cross]]

allBlank :: Int -> Game
allBlank n = Game n $ replicate n (replicate n Blank)

allRows :: Game -> [Row]
allRows (Game _ rows) = rows ++ transpose rows ++ [diagonal rows] ++[diagonal $ map reverse rows]
    where
        diagonal r = [row !! index | (row, index) <- zip r [0..]]

winner :: Game -> (Game, Maybe Mark)
winner g@(Game _ rows)
    | elem [Naught, Naught, Naught] $ allRows g = (g, Just Naught)
    | elem [Cross, Cross, Cross] $ allRows g = (g, Just Cross)
    | otherwise = (g, Nothing)

changeElem :: [[a]] -> (Int, Int) -> a -> [[a]]
changeElem (xs:ys) (0, col) e = [take col xs ++ [e] ++ drop (col + 1) xs] ++ ys
changeElem (xs:ys) (row, col) e = [xs] ++ changeElem ys ((row - 1), col) e


makeMove :: (Mark, Pos) -> Game -> Game
makeMove (mark, pos) (Game size rows) = Game size newRows
    where
        columnIndex = mod size pos
        rowIndex = div (size - columnIndex) size
        newRows = changeElem rows (rowIndex, columnIndex) mark

