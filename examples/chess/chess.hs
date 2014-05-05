module Chess where
	import Probabilities
	import Probabilities.Markov
	import System.Random
	import Control.Monad
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Data.List (lookup)
	import Data.Tuple (swap)

	{--
		Data types and functions that are used by chessboard distributions
	--}

	data Color = Black | White
		deriving (Eq, Show)

	oppositeOf :: Color -> Color
	oppositeOf Black = White
	oppositeOf White = Black

	data PieceType = King | Queen | Knight | Bishop | Rook | Pawn
		deriving (Eq, Show)

	data Piece = Piece { getType :: PieceType, getColor :: Color, getNumber :: Int, isFirstMove :: Bool }
		deriving (Eq, Show)

	type PieceState = (Piece, (Int, Int))

	type ChessState = [PieceState]

	data ChessUpdate = Move { pieceOf :: Piece, getOldPosition :: (Int, Int), getNewPosition :: (Int, Int) } |
		Eat { eatenBy :: Piece, eaten :: Piece, eatenByPosition :: (Int, Int), eatenPosition :: (Int, Int) }
		deriving (Eq, Show)

	updateBoard :: ChessUpdate -> ChessState -> ChessState
	updateBoard (Eat p p' before after) = updateBoard (Move p before after) . filter ((/= p') . fst)
	updateBoard (Move p before after) = map (\(k, v) -> if k == p then (pieceMoved k, after) else (k, v))
		where
			pieceMoved (Piece ptype color number True) = Piece ptype color number False
			pieceMoved p = p

	possibleMoves :: Piece -> ChessState -> [ChessUpdate]
	possibleMoves p cstate = do
		let Just current = lookup p cstate
		let traceHere = trace current
		possibility <- case getType p of
			King -> do
				dx <- [-1, 0, 1]
				dy <- [-1, 0, 1]
				let next = addmove current (dx, dy)
				guard (isSafe next)
				return next
			Pawn -> do
				dx <- [-1, 0, 1]
				dy <- [-2, -1, 1, 2]
				let next = addmove current (dx, dy)
				guard ((dy > 0) <=> (getColor p == White))
				guard ((dx /= 0) ==> (pieceAt next == Just (oppositeOf (getColor p))))
				guard ((abs dy == 2) ==> isFirstMove p)
				return next
			Knight -> do
				dx <- [-2, -1, 1, 2]
				dy <- [-2, -1, 1, 2]
				guard (abs dx /= abs dy)
				return (addmove current (dx, dy))
			Rook -> concat $ do
				dx <- [-1, 0, 1]
				dy <- [-1, 0, 1]
				guard (dx == 0 || dy == 0)
				return $ traceHere (dx, dy)
			Bishop -> concat $ do
				dx <- [-1, 1]
				dy <- [-1, 1]
				return $ traceHere (dx, dy)
			Queen -> concat $ do
				dx <- [-1, 0, 1]
				dy <- [-1, 0, 1]
				return $ traceHere (dx, dy)
		guard (possibility /= current)
		guard (withinboard possibility)
		case lookup possibility (map swap cstate) of
			Just p' -> if (getColor p' == getColor p)
				then []
				else return (Eat p p' current possibility)
			Nothing -> return (Move p current possibility)
			where
				addmove (x, y) (dx, dy) = (x + dx, y + dy)
				withinboard (x, y) = (0 < x) && (x < 9) && (0 < y) && (y < 9)
				isSafe spot = [] == do
					p' <- cstate
					guard ((getColor . fst $ p') /= (getColor p))
					guard ((getType . fst $ p') /= King)
					n <- possibleMoves (fst p') cstate
					guard (spot == (getNewPosition n))
					return n
				trace spot ray = do
					let next = addmove spot ray
					guard (withinboard next)
					maybe (next : (trace next ray)) (\color -> if (color == (getColor p)) then [] else [next]) (pieceAt next)
				pieceAt location = fmap getColor (lookup location (map swap cstate))
				(==>) p q = not (p && (not q))
				(<=>) p q = (p ==> q) && (q ==> p)


	{--
		Some example distributions using the Probabilities library
	--}

	markovChess :: RandomGen r => Piece -> ChessState -> Markov r ChessState
	markovChess p startState = fromTransitionFunction startState transitionFunction
		where
			transitionFunction cstate = do
				move <- uniformSpace $ possibleMoves p cstate
				return (updateBoard move cstate)

	runMarkovChessWalk :: RandomGen r => Int -> Piece -> ChessState -> Distribution r (Int, Int)
	runMarkovChessWalk n p startState = (flip evalStateT) (markovChess p startState) $ do
		replicateM_ n transitionState
		gets (fromJust . lookup p . getCurrentState)
			where
				fromJust (Just x) = x