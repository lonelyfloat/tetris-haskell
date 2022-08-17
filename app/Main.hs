module Main where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Tetromino
import Board
-- NOTE: Standard Tetromino dimensions - 10x20

data GameState = GameState {
    activeTetronimo :: Tetromino,
    board :: Board,
    leaveGame :: Bool,
    timer :: Int
    }

data IOData = IOData {
    strInput :: String,
    randomT :: Maybe Tetromino
    }
initialGame :: GameState 
initialGame = GameState {
        activeTetronimo = Tetromino TShape RotatedDefault (0, 0),
        board = createBoard (20,10), 
        leaveGame = False,
        timer = 0
    }
removeLines :: Int -> IO ()
removeLines x = replicateM_ x (putStr "\x1b[1A\x1b[2K")

getTetroRotation :: Tetromino -> String -> TetrominoRotation
getTetroRotation tetro input
    | input == "q" = if tetroRotation tetro == RotatedRight then RotatedDefault else toEnum (fromEnum (tetroRotation tetro) + 1)
    | input == "e" = if tetroRotation tetro == RotatedDefault then RotatedRight else toEnum (fromEnum (tetroRotation tetro) - 1)
    | otherwise = tetroRotation tetro

update :: GameState -> IOData -> GameState
update state@(GameState tetro board@(Board bt) leave timer) (IOData input rand) = 
    let t = timer + 1 
        x = case input of
            "a" -> -3
            "d" -> 3
            _   -> 0
        tet = tetro {
        tetroRotation = getTetroRotation tetro input,
        tetroPosition = (fst (tetroPosition tetro) + x, snd (tetroPosition tetro) + 1) 
            }
        b = if tetrominoColliding board tet then appendTetromino board tet else board 
        tetr = if snd (tetroPosition tet) >= length bt - 1 || tetrominoColliding board tetro then fromJust rand else tet
    in
    state {activeTetronimo=tetr, board=b, leaveGame = input == "Q",timer=t}

render :: GameState -> IO ()
render (GameState tetro t@(Board board) _ _) = do
    -- Use escape codes to REMOVE lines
    let b = getBoard $ appendTetromino t tetro
    removeLines (length b + 3)
    -- Temporary - just print the board
    putStrLn $ unlines b 
    print . snd . tetroPosition $ tetro

loop :: StateT GameState IO ()
loop = do
    s <- get 
    input <- liftIO getLine
    let st = update s (IOData input (Just $ Tetromino TShape RotatedDefault (5,0) ) )
    liftIO $ render st
    put st
    unless (leaveGame st) loop

main :: IO ()
main = do 
    let i = initialGame 
    let t = filter (\ (x,y) -> getBoard (board initialGame) !! y !! x /= ' ' ) $ getIDs (getBoard $ board initialGame)
    print t
    evalStateT loop initialGame
