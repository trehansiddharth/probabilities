module Chess where
	import Probabilities
	import System.Random
	import Control.Monad
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Data.List (lookup)

	{--
		Simple distributions for knight and king: a single piece starts some place
		on a chessboard and its position after n steps is measured.
	--}

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

	{--
		More complex distributions involving multiple game pieces interacting with each other
		and various strategies of play. A full-fledged game of chess.
	--}

	data Color = Black | White
		deriving (Eq, Show)

	data PieceType = King | Queen | Knight | Bishop | Rook | Pawn { isFirstMove :: Bool }
		deriving (Eq, Show)

	data Piece = Piece { getType :: PieceType, getColor :: Color, getNumber :: Int }
		deriving (Eq, Show)

	type PieceState = (Piece, (Int, Int))

	type ChessState = [PieceState]

	data Move = Move { pieceOf :: Piece, getOldPosition :: (Int, Int), getNewPosition :: (Int, Int) }
		deriving (Eq, Show)

	randomChessWalk :: RandomGen r => Piece -> Int -> StateT ChessState (Distribution r) (Int, Int)
	randomChessWalk p 0 = do
		Just position <- gets (lookup p)
		return position
	randomChessWalk p n = do
		movePieceRandomly p (\s m -> uniformSpace m)
		randomChessWalk p (n - 1)

	applyMove :: Move -> (ChessState -> ChessState)
	applyMove move = case getType . pieceOf $ move of
		Pawn True -> map (\(k, v) -> if k == (pieceOf move) then (pawnMoved k, getNewPosition move) else (k, v)) -- update pawn to indicate that it has been moved already
		_ -> map (\(k, v) -> if k == (pieceOf move) then (k, getNewPosition move) else (k, v))
		where
			pawnMoved (Piece (Pawn True) color number) = Piece (Pawn False) color number

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
		let filledPositions = onlyFilledPositions cstate
		let traceHere = trace (map snd cstate) current		-- partially apply the trace function so it starts from the current position and knows filled positions
		possibility <- case getType p of
			King -> do										-- if p is a King:
				dx <- [-1, 0, 1]							--		x moves are between -1 and 1
				dy <- [-1, 0, 1]							--		y moves are between -1 and 1
				let next = addmove current (dx, dy)			--		define next, the adition of a move to the current position
				guard (isSafe next)							--		check if the king can get eaten at next or not
				return next 								--		return next
			Pawn firstmove -> do							-- if p is a Pawn:
				dy <- if firstmove then [1, 2] else [1]		--		if this is the first move, pawn can move two spaces, otherwise one
				let next = addmove current (0, dy)			--		define next
				return next									--		return next
			Knight -> do 									-- if p is a Knight:
				dz <- [(1, 2), (2, 1), (-1, 2), (-2, 1),
					(-1, -2), (-2, -1), (1, -2), (2, -1)]	--		define dz as all the possible moves a knight can make
				let next = addmove current dz				--		define next
				return next									--		return next
			Rook -> do 										-- if p is a Rook:
				left <- traceHere (-1, 0)					--		trace all possible rays going left, right, up and down that aren't blocked by another piece
				right <- traceHere (1, 0)
				up <- traceHere (0, 1)
				down <- traceHere (0, -1)
				[left, right, up, down]						--		return these rays
			Bishop -> do 									-- if p is a Bishop:
				upleft <- traceHere (-1, 1)					--		trace all diagonal rays
				upright <- traceHere (1, 1)
				downleft <- traceHere (-1, -1)
				downright <- traceHere (1, -1)
				[upleft, upright, downleft, downright]		--		return these rays
			Queen -> do 									-- if p is a Queen:
				left <- traceHere (-1, 0)					--		trace all horizontal, vertical, and diagonal rays
				right <- traceHere (1, 0)
				up <- traceHere (0, 1)
				down <- traceHere (0, -1)
				upleft <- traceHere (-1, 1)
				upright <- traceHere (1, 1)
				downleft <- traceHere (-1, -1)
				downright <- traceHere (1, -1)
				[upleft, upright, downleft, downright,
					left, right, up, down]					--		return these rays
		guard (possibility /= current)						-- not making a move is not a valid move
		guard (withinboard possibility)						-- check if next is within the board
		guard (not (elem possibility filledPositions))		-- check if next is already taken by a piece of the same color or not
		return (Move p current possibility)					-- return this possible move
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
				trace filledPositions spot ray = do
					let next = addmove spot ray
					guard (withinboard next)
					guard (not (elem next filledPositions))
					next : (trace filledPositions next ray)