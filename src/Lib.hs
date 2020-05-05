module Lib
( printBoard
  , choseSide
  , mainLoop
  ) where

--- Imports ---
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Random
import Data.Char

--- data and type definitions ---
data Square =  X | O | B deriving (Eq)
instance Show Square where
  show X = "X"
  show O = "O"
  show B = " "

type Board = [Square]

data Side = XSide | OSide deriving (Eq,Show)
data GameState = GameState {board :: Board
    ,movesNext :: Side
    ,ai :: Side}
    deriving Show

    

newBoard :: Board
newBoard = [B,B,B,B,B,B,B,B,B]
winBoard = [X,B,O,O,X,O,B,B,X]

--- functions ---
choseSide :: Char -> IO Char
choseSide x = if x `elem` ['X','O']
  then  return x
  else  putStrLn "Choose a side (X) or (O)" >> getChar >>= choseSide 

mainLoop s = evalStateT play (initState s) 

play :: StateT GameState IO (Board,Side)
play = 
  do
    s <- get
    if snd $ win (fst $ lastStatus s) (si2sq $ snd $ lastStatus s)
    then return $ lastStatus s
    else execMove 
    where lastStatus x = (board x, opp $ movesNext x)

execMove = 
  do
    st <- get
    if ai st == movesNext st
    then update chooseMove st 
    else update playerPick st 
  where
    update f currState = 
      do
        upd <- liftIO $ f currState
        put GameState{board = upd, movesNext = opp $ movesNext currState
        , ai = ai currState}
        play
blockOpp st = nMoveWin ((si2sq . opp . ai) st) (board st)
chooseMove st = if (not . null) winningMove
                then return $ head winningMove
                else randomMove st
                where winningMove = map fst $ nMoveWin (si2sq (ai st)) (board st)

safeHead :: [a] -> Maybe a
safeHead x | null x = Nothing
           | otherwise     = Just $ head x
randomMove st = do
    newSquare <- (-1 +) <$> randomRIO(1, length blank)  
    return $ replaceSquare (board st) (si2sq (movesNext st)) 
      (snd (blank !! newSquare))
  where blank = filter(\x -> fst x == B) (indexBoard (board st))

indexBoard :: Board -> [(Square,Int)]
indexBoard = flip zip [0 ..]

initState :: Char -> GameState
initState s = GameState{board = newBoard
  , movesNext = XSide
  , ai = if s == 'X' then OSide else XSide}
  
opp :: Side -> Side 
opp x | x == XSide = OSide
      | x == OSide = XSide

--playerPick :: Board -> Square -> AI -> (Board, Square)
playerPick s = do
  printBoard $ board s
  putStrLn "Choose a numbered square"
  t <- getNextChar 
  return $ (take t $ board s) ++ 
    (si2sq(movesNext s):(drop (t + 1) (board s)))

getNextChar :: IO Int
getNextChar = do
  c <- getChar
  if not $ isDigit c
  then getNextChar
  else return $ digitToInt c

fullBoard :: Board -> Bool
fullBoard = all (flip elem [O,X]) 

side2Square x | x == XSide = X
              | x == OSide = O

win :: Board -> Square -> (Square,Bool)
win b s =  (s, or
      --(
        (((\f i -> checkRow (f b i) s) <$> [col,row] <*> [1,2,3])
         ++ ((\f -> checkRow (f b) s) <$> [diag1,diag2])))
      where checkRow x y = all(== y) x
            col b i = (\z -> b!!(i+z)) <$> [-1,2,5] 
            diag1 b = (b!!) <$> [0,4,8]
            diag2 b = (b!!) <$> [2,4,6] 
            row b i = take 3 $ drop ((i-1) * 2) b

--nextMove :: Board -> Side -> [Board]   
nextMove b s = let sqToTest = filter (\x -> fst x == B) $ indexBoard b
      in  map (\x -> (replaceSquare b (si2sq s) (snd x),snd x)) sqToTest
nMoveWin s b = filter(\x -> snd $ win (fst x) s) (nextMove b $ sq2si s)

replaceSquare :: Board -> Square -> Int -> Board
replaceSquare b s x = take x b ++ [s] ++ drop(1 + x) b

gameOverNoWinner :: Board -> Maybe Bool
gameOverNoWinner b = if fullBoard b
      then Just (snd ( win b O) || snd (win b X))
      else Nothing
sq2si :: Square -> Side
sq2si x = if x == O then OSide else XSide

si2sq :: Side -> Square
si2sq x = if x == XSide then X else O         

--- Functions for output ---
makeLine :: Board -> Int -> String
makeLine line index = x ++ " | " ++ y ++ " | " ++ z
  where x = showSquare 0
        y = showSquare 1
        z = showSquare 2
        showSquare pos = if line!!pos == B 
                    then show (index + pos) 
                    else show $ line!!pos

printBoard b = do
  putStrLn $ makeLine (take 3 b) 0
  putStrLn separator
  putStrLn $ makeLine (take 3 $ drop 3 b) 3
  putStrLn separator
  putStrLn $ makeLine (take 3  $ drop 6 b) 6
  where   separator = "---------"
          showSquare line row pos = if line!!pos == B 
            then show (row + pos) 
            else show $ line!!pos

