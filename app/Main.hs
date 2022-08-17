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
    timer :: Double
    }

data IOData = IOData {
    strInput :: String,
    randomT :: Maybe Tetromino
    }
removeLines :: Int -> IO ()
removeLines x = replicateM_ x (putStr "\x1b[1A\x1b[2K")

getTetroRotation :: Tetromino -> String -> TetrominoRotation
getTetroRotation tetro input
    | input == "q" = if tetroRotation tetro == RotatedRight then RotatedDefault else toEnum (fromEnum (tetroRotation tetro) + 1)
    | input == "e" = if tetroRotation tetro == RotatedDefault then RotatedRight else toEnum (fromEnum (tetroRotation tetro) - 1)
    | otherwise = tetroRotation tetro

update :: GameState -> IOData -> GameState
update state@(GameState tetro board leave timer) (IOData input rand) = 
    let t = timer + 1 
        tet = tetro {
        tetroRotation = getTetroRotation tetro input,
        tetroPosition = (fst (tetroPosition tetro), snd (tetroPosition tetro) + 1) 
            }
        b = board --if tetrominoColliding board tet then appendTetromino board tetro else board 
        tetr = if tetrominoColliding board tetro then fromJust rand else tet
    in
    state {activeTetronimo=tetr, board=b, leaveGame = input == "x",timer=t}

render :: GameState -> IO ()
render (GameState tetro (Board board) _ _) = do
    -- Use escape codes to REMOVE lines
    removeLines (length board)
    -- Temporary - just print the board
    print board 

loop :: StateT GameState IO ()
loop = do
    s <- get 
    let input = ""
    let st = update s (IOData input Nothing)
    liftIO $ render st
    put st
    unless (leaveGame st) loop

main :: IO ()
main = do
    let b = createBoard (10, 20) 
    let tetro = Tetromino TShape RotatedLeft (5,1)
    print (appendTetromino b tetro)
