module Tetromino where

type Position = (Int, Int)

data TetrominoShape = TShape | LShape | ZigzagShape | LineShape | SquareShape deriving (Ord, Eq, Enum, Show)

data TetrominoRotation = RotatedDefault | RotatedLeft | RotatedDown | RotatedRight deriving (Ord, Eq, Enum, Show)

data Tetromino = Tetromino {
    tetroShape :: TetrominoShape,
    tetroRotation :: TetrominoRotation,
    tetroPosition :: Position
   }

instance Show Tetromino where
    show tetro = unlines (getTetrominoBody tetro)

-- Complete array of base shapes 
shapes :: [[String]]
shapes = [
    ["TTT",
     " T ",
     " T "
    ],
    ["L  ",
     "L  ",
     "L  ",
     "LLL"
    ],
    [
     "ZZ ",
     " ZZ"
    ],
    [
     "I",
     "I",
     "I",
     "I"
    ],
    [
     "OO",
     "OO"
    ] ]

-- Utility function for the rotations :D
applyTransform :: [[a]] -> Int -> Int -> [[a]]
applyTransform ls chx chy = [[ls !! abs (chx - x) !! abs(chy - y) | x <- [0..length ls - 1] ] | y <- [0..length (head ls) - 1]] 

-- Function to rotate a matrix, effectively.
rotateMatrix :: [[a]] -> TetrominoRotation -> [[a]]
rotateMatrix body RotatedDefault = body
rotateMatrix body RotatedLeft = applyTransform body 0 (length (head body) - 1)
rotateMatrix body RotatedDown = reverse body
rotateMatrix body RotatedRight = applyTransform body (length body - 1) 0

-- Gets [[Char]] of tetromino (for board) with rotations and everything except position
getTetrominoBody :: Tetromino -> [[Char]]
getTetrominoBody tetro = rotateMatrix (shapes !! (fromEnum . tetroShape) tetro) (tetroRotation tetro)


