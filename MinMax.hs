module MinMax where
import GameStructure
import Interaction
import Data.List
import Test.QuickCheck
import Data.Maybe

data Tree a = Node a [Tree a] deriving (Show, Eq)

getPoints :: Tree (Int, Game) -> Int
getPoints (Node (n, _) _) = n

getGame :: Tree (Int, Game) -> Game
getGame (Node (_, g) _) = g
 
makeTree :: Game -> Mark -> Tree Game
makeTree g@(Game {size = size, rows = rows}) mark = case not ( any (==Blank) $ concat rows) of
    True    -> Node g [] -- When all squares are marked, the game has terminated
    _       -> Node g (map (\x -> (makeTree x mark)) (allMoves g (oppositeMark mark)))


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
-- MinMax algoritm:
-- Gets AI:s mark and calcs points to each move
calcMinMax :: Tree Game -> Mark -> Tree (Int, Game)
calcMinMax (Node game list) mark = case list of
    []  -> case winner game of
        Just a  -> if a == mark then Node (1, game) [] else Node (-1, game) []
        _       -> Node (0, game) []
    _   -> Node (sumPoints, game) rem
    where
        rem = map (\x -> calcMinMax x mark) list
        sumPoints = foldr (\(Node (x, y) _) acc -> acc + x) 0 rem


-- Gets the best possible move for a game and a mark
bestMove :: Game -> Mark -> Game
bestMove g@(Game {size = size, rows = rows}) mark = maxGame
    where
        pointsTree = calcMinMax (makeTree g mark) mark
        maxGame = go pointsTree []

        go :: Tree (Int, Game) -> [(Int, Game)] -> Game
--        go (Node (int, game) list) list2 = error $ "int =" ++ show int ++ " game = " ++ show game ++ " list = " ++ show list ++ " list2 = " ++ show list2
        go (Node _ []) [(accPoints, accGame)] = accGame
        go (Node p (x:xs)) [] = go (Node p xs) [(getPoints x, getGame x)]
        go (Node p (x:xs)) [(accPoints, accGame)] 
            | getPoints x >= accPoints = go (Node p xs) [(getPoints x, getGame x)]
            | otherwise = go (Node p xs) [(accPoints, accGame)]


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


main :: IO ()
main = do 
    putStrLn (show (createGame 3))
    putStrLn "Lets play 3 i rad you are Naught and Computer is Cross \nWhats your move? Row: (1-3)"
    y <- readLn
    putStrLn "Collom: (1-3):"
    x <- readLn
    p <- generate (makeRandomMove (makeMove Naught (y-1,x-1) (createGame 3)))
    play p
    
play :: Game -> IO ()
play game = do 
    case winner game of
        Nothing -> do
            putStrLn $ (show game) ++ "\nWhats your move? Row: (1-3)"
            y <- readLn
            putStrLn "Collom: (1-3):"
            x <- readLn
            case winner (makeMove Naught (y-1,x-1) game) of
                Just Naught -> putStrLn $ show (makeMove Naught (y-1,x-1) game) ++ "\nYou won congrats"
                Nothing -> do -- Behöver ett scenario för No Winner här
                    newGame <- generate (makeRandomMove (makeMove Naught (y-1,x-1) game))
                    play newGame
        Just Cross -> putStrLn $ (show game) ++ "\nComputer won"
        

makeRandomMove :: Game -> Gen (Game)
makeRandomMove game = do
    n <- choose (0, length (allnextMove game (allMoves (createGame 3) Cross))) -- (length (allnextMove game (allMoves (createGame 3) Naught))))
    return ((allnextMove game (allMoves (createGame 3) Cross)) !! n)

