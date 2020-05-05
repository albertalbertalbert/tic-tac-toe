module Main where

import Lib

main :: IO ()
main = do
        s <- chooseSide ' '
        putStrLn $ "You chose:\t" ++ show s
        (x,y) <- mainLoop s
        putStrLn "Game Over.  Thanks for playing!"
        putStrLn "--------------"
        putStrLn ""
        putStrLn  $ "The game was won by\t" ++ show y
        putStrLn ""
        printBoard x

