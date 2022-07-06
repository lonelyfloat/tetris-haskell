module Main where

import Data.List

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

removeLines :: Int -> IO ()
removeLines x = replicateM_ x (putStr "\x1b[1A\x1b[2K")

update :: GameState -> String -> GameState
update state@(GameState tetro board leave timer) input = 
    let t = timer + 1 
        b = if tetrominoColliding board tetro then appendTetromino board tetro else board 
    in
    state {activeTetronimo=tetro, board=b, leaveGame = input == "x",timer=t}

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
    let st = update s input
    liftIO $ render st
    put st
    unless (leaveGame st) loop

main :: IO ()
main = do
    let b = createBoard (10, 20) 
    let tetro = Tetromino TShape RotatedLeft (5,1)
    print (appendTetromino b tetro)
