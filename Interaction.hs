module Interaction where
import GameStructure

showGameInputs :: Game -> String
showGameInputs (Game _ rows) = unlines $ ["abc"] ++ [unwords [show mark | mark <- row]| row <- rows]


getInput :: Game -> Mark -> IO Game
getInput (Game _ rows) mark = undefined
--    putStrLn $ "It is " ++ show mark ++ "s turn!"
