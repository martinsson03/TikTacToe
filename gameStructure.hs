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

testGame :: Game
testGame = Game [[Naught, Naught, Cross], [Blank, Cross, Naught], [Blank, Cross, Cross]]

allRows :: Game -> [Row]
allRows (Game rows) = rows ++ transpose rows ++ [diagonal rows] ++[diagonal $ map reverse rows]
    where
        diagonal r = [row !! index | (row, index) <- zip r [0..]]