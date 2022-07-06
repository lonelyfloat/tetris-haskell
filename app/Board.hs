module Board where

import Data.List
import Tetromino

newtype Board = Board [[Char]]

instance Show Board where
    show (Board board) = unlines board

boardDimensions :: Board -> (Int, Int)
boardDimensions (Board board) = (length (head board), length board)

createBoard :: (Int, Int) -> Board
createBoard (x,y) = Board $ replicate y (replicate x ' ')

tetrominoColliding :: Board -> Tetromino -> Bool
tetrominoColliding board tetro = error "Tetromino collision not implemented"

appendTetromino :: Board -> Tetromino -> Board
appendTetromino (Board board) tetro = Board [ [ getCharAtPos board tetro (x,y) | x <- [0..length (head board) - 1]] | y <- [0..length board - 1] ]
  where getCharAtPos board tetro pos = let translatedPos = (fst pos - fst (tetroPosition tetro), snd pos - snd (tetroPosition tetro)) in                safeIndex (lines $ show tetro) translatedPos pos board
        safeIndex arr ix abs back
            | fst ix < 0 || fst ix > (length . head) arr - 1 = (back !! snd abs) !! fst abs 
            | snd ix < 0 || snd ix > length arr - 1 = (back !! snd abs) !! fst abs 
            | otherwise = arr !! snd ix !! fst ix
