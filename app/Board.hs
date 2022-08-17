{-# LANGUAGE TupleSections #-}
module Board where

import Data.List
import Tetromino
import Data.Monoid

newtype Board = Board [[Char]]

instance Show Board where
    show (Board board) = unlines board

getBoard :: Board -> [[Char]]
getBoard (Board b) = b

boardDimensions :: Board -> (Int, Int)
boardDimensions (Board board) = (length (head board), length board)

createBoard :: (Int, Int) -> Board
createBoard (x,y) = Board $ replicate y (replicate x ' ')

getIDs :: [[a]] -> [(Int, Int)]
getIDs mat = concatMap (\ls -> map (,ls) [0..((-1+) . length . head $ mat)]) [0..(length mat - 1)] 

tetrominoColliding :: Board -> Tetromino -> Bool
tetrominoColliding (Board b) tetro@(Tetromino _ _ pos@(tx,ty) ) = getAny (anyAre (map (\ (x,y) -> (tx + x, ty + x) ) tetroIDs) boardIDs)
  where filterGetIDs xs = filter (\ (x,y) -> xs !! y !! x /= ' ') (getIDs xs)
        tetroIDs = filterGetIDs . getTetrominoBody $ tetro
        boardIDs = filterGetIDs b
        anyAre [] [] = Any False
        anyAre [] a = Any False
        anyAre (x:xs) a = Any (x `elem` a || snd x >= length b - 2) <> anyAre xs a

appendTetromino :: Board -> Tetromino -> Board
appendTetromino (Board board) tetro = Board [ [ getCharAtPos board tetro (x,y) | x <- [0..length (head board) - 1]] | y <- [0..length board - 1] ]
  where getCharAtPos board tetro pos = let translatedPos = (fst pos - fst (tetroPosition tetro), snd pos - snd (tetroPosition tetro)) in                safeIndex (lines $ show tetro) translatedPos pos board
        safeIndex arr ix abs back
            | fst ix < 0 || fst ix > (length . head) arr - 1 = (back !! snd abs) !! fst abs 
            | snd ix < 0 || snd ix > length arr - 1 = (back !! snd abs) !! fst abs 
            | otherwise = arr !! snd ix !! fst ix
