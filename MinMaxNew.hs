module MinMax where
import GameStructure
import Interaction
import Data.List
import Test.QuickCheck
import Data.Maybe

data Tree a = Node a [Tree a] deriving (Show, Eq)

makeTree :: Game -> Mark -> (Tree Game)
makeTree Game {size = size, rows = rows} mark = undefined


allMoves :: Game -> Mark -> [Game]
allMoves Game {size = size, rows = rows} mark = go (0,0) (Game size rows) mark
    where
        go :: (Int, Int) -> Game -> Mark -> [Game]
        go (y, x) g@(Game size rows) mark
            | x == size = go (y + 1, 0) (Game size rows) mark
            | y == size = []
            | otherwise = case isBlank g (y, x) of
                True        -> [makeMove mark (y, x) g] ++ go (y, x + 1) g mark
                _           -> go (y, x + 1) g mark

allnextMove :: Game -> [Game] -> [Game]
allnextMove currentgame allnewmoves =  removeItem currentgame (nub [combine currentgame nmove | nmove <- (allnewmoves)])
    where 
        removeItem _ [] = []
        removeItem x (y:ys) | x == y = removeItem x ys
            | otherwise = y : removeItem x ys

-- allNaught = allMoves game Cross
-- allCross = allMoves game Naught
-- exempel på ett scenario som visar alla möjliga drag för Cross när Naught har lagt en cirkel i mitten:
-- allnextMove (makeMove Naught (1,1) (createGame 3)) (allMoves (createGame 3) Cross)

zipRowWith :: (Mark -> Mark -> Mark) -> Game -> Game -> [[Mark]]
zipRowWith f (Game size rows) (Game size2 rows2)
  = zipWith (zipWith (\r1 r2 -> f r1 r2)) rows rows2

combine :: Game -> Game -> Game
combine (Game size rows) g2 = Game size $ zipRowWith together (Game size rows) g2

together :: Mark -> Mark -> Mark
together Blank Blank    = Blank
together Blank Cross    = Cross
together Naught Blank   = Naught
together x y            = x 


playGame :: IO ()
playGame = do 
    putStrLn (show (createGame 3))
    putStrLn "Lets play 3 i rad you are Naught and Computer is Cross \nGive a coordinate to place (0-2,0-2):"
    pos <- readLn
    p <- generate (makeRandomMove (makeMove Naught pos (createGame 3)))
    play p
    
play :: Game -> IO ()
play game = do 
    case winner game of
        Nothing -> do
            putStrLn ("The computer played\n" ++ show game ++ "\n Whats your move? Row: 0-2 Collom: 0-2)\n")
            pos <- readLn
            case winner (makeMove Naught pos game) of
                Just Naught -> putStrLn "You won congrats"
                Nothing -> do
                    newGame <- generate (makeRandomMove (makeMove Naught pos game))
                    play newGame
        Just Cross -> putStrLn "Computer won"
        

makeRandomMove :: Game -> Gen (Game)
makeRandomMove game = do
    n <- choose (0, length (allnextMove game (allMoves (createGame 3) Cross))) -- (length (allnextMove game (allMoves (createGame 3) Naught))))
    return ((allnextMove game (allMoves (createGame 3) Cross)) !! n)