module Chess where
	import Probabilities
	import System.Random
	import Control.Monad
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Data.List (lookup)
	import Data.Tuple (swap)

	data Color = Black | White
		deriving (Eq, Show)

	data PieceType = King | Queen | Knight | Bishop | Rook | Pawn { isFirstMove :: Bool }
		deriving (Show)

	instance Eq PieceType where
		(==) King King = True
		(==) Queen Queen = True
		(==) Knight Knight = True
		(==) Bishop Bishop = True
		(==) Rook Rook = True
		(==) (Pawn _) (Pawn _) = True
		(==) _ _ = False

	data Piece = Piece { getType :: PieceType, getColor :: Color, getNumber :: Int }
		deriving (Eq, Show)

	type PieceState = (Piece, (Int, Int))

	type ChessState = [PieceState]

	data Move = Move { pieceOf :: Piece, getOldPosition :: (Int, Int), getNewPosition :: (Int, Int) } |
		Eat { eatenBy :: Piece, eaten :: Piece, eatenByPosition :: (Int, Int), eatenPosition :: (Int, Int) }
		deriving (Eq, Show)

	randomChessWalk :: RandomGen r => Piece -> Int -> StateT ChessState (Distribution r) (Int, Int)
	randomChessWalk p 0 = do
		Just position <- gets (lookup p)
		return position
	randomChessWalk p n = do
		movePieceRandomly p (\s m -> uniformSpace m)
		randomChessWalk p (n - 1)

	applyMove :: Move -> ChessState -> ChessState
	applyMove (Eat p p' before after) = applyMove (Move p before after) . filter ((/= p') . fst)
	applyMove (Move p before after) = map (\(k, v) -> if k == p then (pawnMoved k, after) else (k, v))
		where
			pawnMoved (Piece (Pawn True) color number) = Piece (Pawn False) color number
			pawnMoved p = p

	movePieceRandomly :: RandomGen r => Piece -> (ChessState -> [Move] -> Distribution r Move) -> StateT ChessState (Distribution r) Move
	movePieceRandomly p d = do
		Just current <- gets (lookup p)					-- get location of piece p
		cstate <- get									-- extract the current state
		let moves = possibleNexts p cstate				-- enumerate all the possible valid moves that the piece can make
		move <- lift . d cstate $ moves					-- from the list of valid moves, choose one randomly according to distribution d
		modify (applyMove move)							-- modify the state of the chessboard using the move
		return move										-- return the move that was made

	possibleNexts :: Piece -> ChessState -> [Move]
	possibleNexts p cstate = do
		let Just current = lookup p cstate
		let piecePositions = map swap cstate
		let traceHere = trace piecePositions current (getColor p)
		possibility <- case getType p of
			King -> do										-- if p is a King:
				dx <- [-1, 0, 1]							--		x moves are between -1 and 1
				dy <- [-1, 0, 1]							--		y moves are between -1 and 1
				let next = addmove current (dx, dy)			--		define next, the adition of a move to the current position
				guard (isSafe next)							--		check if the king can get eaten at next or not
				return next 								--		return next
			Pawn firstmove -> do							-- if p is a Pawn:
				dy <- if firstmove then [1, 2] else [1]		--		if this is the first move, pawn can move two spaces, otherwise one
				return (addmove current (0, dy))			--		return next
			Knight -> do 									-- if p is a Knight:
				dx <- [-2, -1, 1, 2]
				dy <- [-2, -1, 1, 2]
				guard (abs dx /= abs dy)
				return (addmove current (dx, dy))
			Rook -> concat $ do 							-- if p is a Rook:
				dx <- [-1, 0, 1]
				dy <- [-1, 0, 1]
				guard (dx == 0 || dy == 0)
				return $ traceHere (dx, dy)
			Bishop -> concat $ do 							-- if p is a Bishop:
				dx <- [-1, 1]
				dy <- [-1, 1]
				return $ traceHere (dx, dy)
			Queen -> concat $ do							-- if p is a Queen:
				dx <- [-1, 0, 1]
				dy <- [-1, 0, 1]
				return $ traceHere (dx, dy)
		guard (possibility /= current)						-- not making a move is not a valid move
		guard (withinboard possibility)						-- check if next is within the board
		case lookup possibility piecePositions of 			-- check if this possible move is taken up by another piece or not
			Just p' -> if (getColor p' == getColor p)
				then []										-- if it is of the same color, it is not a valid move
				else return (Eat p p' current possibility)	-- if it is of another color, then this piece is eaten
			Nothing -> return (Move p current possibility)	-- if there is no piece on this possible spot, then it is okay to move there
			where
				addmove (x, y) (dx, dy) = (x + dx, y + dy)
				withinboard (x, y) = (0 < x) && (x < 9) && (0 < y) && (y < 9)
				onlyFilledPositions = map snd . filter ((== (getColor p)) . getColor . fst)
				isSafe spot = [] == do
					p' <- cstate
					guard ((getColor . fst $ p') /= (getColor p))
					guard ((getType . fst $ p') /= King)
					n <- possibleNexts (fst p') cstate
					guard (spot == (getNewPosition n))
					return n
				trace piecePositions spot mycolor ray = do
					let next = addmove spot ray
					guard (withinboard next)
					case (lookup next piecePositions) of
						Just (Piece _ color _) -> if (color == mycolor)
							then []
							else [next]
						Nothing -> next : (trace piecePositions next mycolor ray)