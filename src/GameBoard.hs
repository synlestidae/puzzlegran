module GameBoard(board) where 

import Data.Matrix

newtype Board = Board (Matrix Int) deriving Show

data Move = MUp | MDown | MLeft | MRight

board :: Board
board = Board (fromList 7 7 [
	-1,-1,1,1,1,-1,-1,
	-1,-1,1,1,1,-1,-1,
	 1, 1,1,1,1, 1, 1,
	 1, 1,1,0,1, 1, 1,
	 1, 1,1,1,1, 1, 1,
	-1,-1,1,1,1,-1,-1,
	-1,-1,1,1,1,-1,-1])

dest :: Int -> Int -> Move -> (Int, Int)
dest x y MUp = (x,y+2)
dest x y MRight = (x+2,y)
dest x y MDown = (x,y-2)
dest x y MLeft = (x-2,y)


middle :: Int -> Int -> Move -> (Int, Int)
middle x y MUp = (x,y+1)
middle x y MRight = (x+1,y)
middle x y MDown = (x,y-1)
middle x y MLeft = (x-1,y)

instance Eq Board where
	(==) (Board matrixA) (Board matrixB) = any (== matrixA) $ map (rotateBy matrixB) [0,1,2,3]

rotateBy :: Matrix Int -> Int -> Matrix Int
rotateBy matrix 0  = matrix
rotateBy matrix n  = multStd rotationMatrix $ transpose (rotateBy matrix (n-1))
	where rotationMatrix = fromList 7 7 ([0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0] :: [Int])

moveMarble :: Board -> Int -> Int -> Move -> [Board]
moveMarble (Board board) x y move 
	| x >= 7 || y >= 7 || x < 0 || y < 0 = []
	| getElem x'' y'' board == 0 && getElem x' y' board == 1 &&
		getElem x y board == 1 = [
			Board (setElem 1 (x'',y'') $ setElem 0 (x', y') $  setElem 0 (x,y) board)
		]
	| otherwise = []
	where 
		(x', y') = middle x y move
		(x'', y'') = dest x y move