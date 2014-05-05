module Chess where
	import Prelude hiding (id, (.))
	import Probabilities
	import Probabilities.Markov
	import System.Random
	import Control.Monad
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Data.List (lookup)
	import Data.Tuple (swap)
	import Control.Category

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
				guard ((dy > 0) <=> (getColor p == Black))
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

	standardStartState :: ChessState
	standardStartState = whitePieces ++ blackPieces
		where
			whitePawns = map (\i -> (Piece Pawn White i True, (i, 7))) [1 .. 8]
			reflectx (p, (x, y)) = (p, (9 - x, y))
			reflecty (p, (x, y)) = (p, (x, 9 - y))
			increment (p, z) = (p { getNumber = 1 + getNumber p }, z)
			oppose (p, z) = (p { getColor = oppositeOf (getColor p) }, z)
			rook = (Piece Rook White 1 True, (1, 8))
			knight = (Piece Knight White 1 True, (2, 8))
			bishop = (Piece Bishop White 1 True, (3, 8))
			queen = (Piece Queen White 1 True, (4, 8))
			king = (Piece King White 1 True, (5, 8))
			whitePieces = whitePawns ++ [rook, knight, bishop, queen, king] ++ (map (increment . reflectx) [rook, knight, bishop])
			blackPieces = map (oppose . reflecty) whitePieces

	{--
		Some example distributions using the Probabilities and Probabilities.Markov
		modules.
	--}

	markovChess :: RandomGen r => Piece -> ChessState -> Markov r ChessState
	markovChess p startState = fromTransitionFunction startState $ \cstate -> do
		move <- uniformSpace $ possibleMoves p cstate
		return (updateBoard move cstate)

	runMarkovChessWalk :: RandomGen r => Int -> Piece -> ChessState -> Distribution r (Int, Int)
	runMarkovChessWalk n p startState = (flip evalStateT) (markovChess p startState) $ do
		replicateM_ n transitionState
		gets (fromJust . lookup p . getCurrentState)
			where
				fromJust (Just x) = x

	data Lens a b = Lens (a -> b) (a -> b -> a)

	onlyFst = Lens fst (\(x, y) x' -> (x', y))
	onlySnd = Lens snd (\(x, y) y' -> (x, y'))

	instance Category Lens where
		id = Lens id const
		(.) (Lens split1 merge1) (Lens split2 merge2) = Lens (split1 . split2) (\state part -> merge2 state (merge1 (split2 state) part))

	with :: Monad m => Lens s s' -> StateT s' m a -> StateT s m a
	with (Lens split merge) f = do
		state <- get
		result <- lift $ runStateT f (split state)
		put (merge state (snd result))
		return (fst result)