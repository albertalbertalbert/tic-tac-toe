module Main where

import TicTacToe

main :: IO ()
main = do
        s <- chooseSide ' '
        putStrLn $ "You chose:\t" ++ show s
        (x,y) <- mainLoop s
        putStrLn "Game Over.  Thanks for playing!"
        putStrLn "--------------\n"
        putStrLn  $ "The game was won by " ++ show y ++ "\n"
        printBoard x

