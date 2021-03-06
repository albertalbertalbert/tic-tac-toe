module TicTacToe
( printBoard
  , chooseSide
  , mainLoop
  , win
  , Square ( .. )
  ) where

--- Imports ---
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Random
import Data.Char
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Applicative

--- data and type definitions ---
data Square =  X | O | B deriving (Eq,Show)

type Board = [Square]

data Side = XSide | OSide deriving (Eq,Show)
data GameState = GameState {board :: Board
    ,movesNext :: Side
    ,ai :: Side}
    deriving Show

newBoard :: Board
newBoard = [B,B,B,B,B,B,B,B,B]
winBoard = [X,B,O,O,X,O,B,B,X]
type MoveResult a = MaybeT (StateT GameState IO) a
--- functions ---

mainLoop = evalStateT play . initState

chooseSide :: Char -> IO Char
chooseSide x = if toUpper x `elem` ['X','O']
  then  return $ toUpper x
  else  putStrLn "Choose a side (X) or (O)" >> getChar >>= chooseSide 

play' :: MoveResult (Board, Side)
play' = undefined

performState :: StateT GameState IO a -> MoveResult a
performState = lift

play :: StateT GameState IO (Board,Side)
play = 
  do
    s <- get
    if snd $ win (fst $ lastStatus s) (si2sq $ snd $ lastStatus s)
    then return $ lastStatus s
    else execMove 
    where lastStatus s = (board s, opp $ movesNext s)

execMove = 
  do
    st <- get
    if ai st == movesNext st
    then update chooseMove' st 
    else update playerPick st 
  where
    update f st = 
      (liftIO $ f st) >>= \x -> put GameState{board = x
        , movesNext = opp $ movesNext st, ai = ai st} >> play

blockOpp st = safeHead (map snd $ nMoveWin ((si2sq . opp . ai) st) (board st))

winNext st = safeHead $ map snd $ nMoveWin ((si2sq . ai) st) (board st)

chooseMove' st = let  choice = winNext st <|> blockOpp st
                      next = replEl (board st) (si2sq (ai st)) 
  in case choice of Just k -> return $ next k
                    Nothing -> do
                                  k <- randomMove' st 
                                  return $ next k

safeHead :: [a] -> Maybe a
safeHead x | null x     = Nothing
           | otherwise  = Just $ head x

randomMove' :: GameState -> IO Int
randomMove' st = do
  s <- randomRIO(0,length blank -1) 
  return $ snd ( blank !! s )
  where blank = openSquares st

openSquares st = filter ((== B) . fst) (indexBoard $ board st)

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
  t <- putStrLn "Choose a numbered square" >> getNextSq 
  return $ replEl (board s) (si2sq$movesNext s) t

getNextSq :: IO Int
getNextSq = do
  c <- getChar
  if not $ isDigit c
  then getNextSq
  else return $ digitToInt c

fullBoard :: Board -> Bool
fullBoard = all (`elem` [O,X]) 

win :: Board -> Square -> (Square,Bool)
win b s =  (s, or
        (((\f i -> checkRow s (f b i) ) <$> [col,row] <*> [1,2,3])
         ++ ((\f -> checkRow s (f b)) <$> [diag1,diag2])))
      where checkRow sq = all (== sq) 
            col b i = (\z -> b!!(i+z)) <$> [-1,2,5] 
            diag1 b = (b!!) <$> [0,4,8]
            diag2 b = (b!!) <$> [2,4,6] 
            row b i = take 3 $ drop ((i-1) * 3) b

nextMove :: Board -> Side -> [(Board, Int)]   
nextMove b s = let sqToTest = filter (\x -> fst x == B) $ indexBoard b
      in  map (\x -> (replEl b (si2sq s) (snd x),snd x)) sqToTest

nMoveWin :: Square -> Board -> [(Board, Int)]   
nMoveWin s b = filter(\x -> snd $ win (fst x) s) (nextMove b $ sq2si s)

replEl :: [a] -> a -> Int -> [a]
replEl xs x i = take i xs ++ [x] ++ drop(1 + i) xs

gameOverNoWinner :: Board -> Maybe Bool
gameOverNoWinner b = if fullBoard b
      then Just $ not (snd ( win b O) || snd (win b X))
      else Nothing

sq2si :: Square -> Side
sq2si x = if x == O then OSide else XSide

si2sq :: Side -> Square
si2sq x = if x == XSide then X else O         

--- Functions for output ---
makeLine :: Board -> Int -> String
makeLine line row = x ++ " | " ++ y ++ " | " ++ z
  where x = showSquare 0
        y = showSquare 1
        z = showSquare 2
        showSquare pos = if line!!pos == B 
                    then show (row + pos) 
                    else show $ line!!pos

printBoard :: Board -> IO()
printBoard b = do
  putStrLn $ makeLine (take 3 b) 0
  putStrLn separator
  putStrLn $ makeLine (take 3 $ drop 3 b) 3
  putStrLn separator
  putStrLn $ makeLine (take 3  $ drop 6 b) 6
  where   separator = "---------"

