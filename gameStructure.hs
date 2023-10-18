module GameStructure where

import Data.Char
import Data.List


data Mark = Cross | Naught | Blank deriving Eq
type Row = [Mark]
data Game = Game {rows :: [Row]}

instance Show Mark where
    show mark = case mark of
        Cross   -> "X"
        Naught  -> "O"
        Blank   -> "_"


instance Show Game where
    show (Game rows) = unlines [unwords [show mark | mark <- row]| row <- rows]

gameSize :: Game -> (Int, Int)
gameSize (Game rows) = (length rows, length $ rows !! 0)

createGame :: (Int, Int) -> Game
createGame (y, x) = Game $ replicate y $ replicate x Blank

testGame :: Game
testGame = Game [[Naught, Naught, Cross], [Blank, Cross, Naught], [Blank, Cross, Cross]]

allRows :: Game -> [Row]
allRows (Game rows) = rows ++ transpose rows ++ [diagonal rows] ++[diagonal $ map reverse rows]
    where
        diagonal r = [row !! index | (row, index) <- zip r [0..]]

winner :: Game -> (Game, Maybe Mark)
winner g@(Game rows)
    | elem [Naught, Naught, Naught] $ allRows g = (g, Just Naught)
    | elem [Cross, Cross, Cross] $ allRows g = (g, Just Cross)
    | otherwise = (g, Nothing)
