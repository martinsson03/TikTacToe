module Interaction where
import GameStructure

showGameInputs :: Game -> String
showGameInputs (Game rows) = unlines $ ["abc"] ++ [unwords [show mark | mark <- row]| row <- rows]


getInput :: Game -> Mark -> IO Game
getInput (Game rows) mark = undefined
--    putStrLn $ "It is " ++ show mark ++ "s turn!"
