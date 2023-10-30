import GameStructure
import MinMax
import Data.Char



mainn :: IO ()
mainn = do
    putStrLn "Welcome to TicTacToe!\nHow big game do you want to play?"
    size <- readLn
    putStrLn "What mark do you want to play as?"
    input <- getLine
    let mark = readMark input
    (winGame, _) <- repeteGame 0 (allBlank size, mark)
    case winner winGame of
        Just a      -> do
            putStrLn $ show a ++ " is the winner!!"
        _           ->
            putStrLn "Its a tie!"
        


repeteGame :: Int -> (Game, Mark) -> IO (Game, Mark)
repeteGame n (game, mark) = do
    if (isFull game) then return (game, mark) else
        if (winner game == Just Naught) || (winner game == Just Cross) then return (game, mark) else
            case even n of
                True    -> do
                    result <- makeMoveUser (game, mark)
                    repeteGame (n + 1) (result, oppositeMark mark)
                _       -> repeteGame (n + 1) (makeMoveAi (game, mark), oppositeMark mark)

makeMoveUser :: (Game, Mark) -> IO Game
makeMoveUser (game, mark) = do
    print game
    putStrLn $ "Make a move! You are: " ++ show mark
    input <- readLn
    return (makeMove mark input game)

makeMoveAi :: (Game, Mark) -> Game
makeMoveAi (game, mark) = bestMove game mark