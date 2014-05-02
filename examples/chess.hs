module Chess where
	import Probabilities
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Data.List (lookup)

	-- Distribution over the position of a chess piece on an 8 by 8 chessboard with a list of valid moves "moves" after "n" moves given that it starts at "starting"
	piece :: RandomGen r => [(Int, Int)] -> (Int, Int) -> Int -> Distribution r (Int, Int)
	piece moves starting 0 = return starting
	piece moves starting n = do
		move <- uniformSpace moves
		let next = addmove starting move
		if validposition next then piece moves next (n - 1) else piece moves starting n
			where
				addmove (x, y) (dx, dy) = (x + dx, y + dy)
				validposition (x, y) = (0 < x) && (x < 9) && (0 < y) && (y < 9)

	-- Distribution over the position of a knight on a chessboard
	knight :: RandomGen r => (Int, Int) -> Int -> Distribution r (Int, Int)
	knight = piece [(1, 2), (2, 1), (-1, 2), (-2, 1), (-1, -2), (-2, -1), (1, -2), (2, -1)]

	-- Distribution over the position of a king on a chessboard
	king :: RandomGen r => (Int, Int) -> Int -> Distribution r (Int, Int)
	king = piece [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

	data Color = Black | White
		deriving (Eq, Show)

	data PieceType = King | Queen | Knight | Bishop | Rook | Pawn
		deriving (Eq, Show)

	data Piece = Piece { getType :: PieceType, getColor :: Color, getNumber :: Int }
		deriving (Eq, Show)

	type PieceState = (Piece, (Int, Int))

	type ChessState = [PieceState]

	pieceUniform :: RandomGen r => Piece -> StateT ChessState (Distribution r) (Int, Int)
	pieceUniform p = do
		Just (x, y) <- gets (lookup p)
		filledPositions <- gets (map snd . filter ((== (getColor p)) . getColor . fst))
		let nexts = case getType p of
			King -> [spot |
				dx <- [-1, 0, 1],
				dy <- [-1, 0, 1],
				let spot = addmove (x, y) (dx, dy),
				withinboard spot,
				not (elem spot filledPositions)]
		next <- lift . uniformSpace $ nexts
		modify (map (\(k, v) -> if k == p then (k, next) else (k, v)))
		return next
			where
				addmove (x, y) (dx, dy) = (x + dx, y + dy)
				withinboard (x, y) = (0 < x) && (x < 9) && (0 < y) && (y < 9)