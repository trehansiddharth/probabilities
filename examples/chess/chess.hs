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
		deriving (Eq, Show, Read)

	data Piece = Piece { getType :: PieceType, getColor :: Color, getNumber :: Int, isFirstMove :: Bool }
		deriving (Eq, Show)

	type PieceState = (Piece, (Int, Int))

	type ChessState = [PieceState]

	data ChessUpdate = Move { pieceOf :: Piece, getOldPosition :: (Int, Int), getNewPosition :: (Int, Int) } |
		Eat { eatenBy :: Piece, eaten :: Piece, eatenByPosition :: (Int, Int), eatenPosition :: (Int, Int) }
		deriving (Eq, Show)

	applyUpdate :: ChessUpdate -> ChessState -> ChessState
	applyUpdate (Eat p p' before after) = applyUpdate (Move p before after) . filter ((/= p') . fst)
	applyUpdate (Move p before after) = map (\(k, v) -> if k == p then (pieceMoved k, after) else (k, v))
		where
			pieceMoved (Piece ptype color number True) = Piece ptype color number False
			pieceMoved p = p

	updateBoard :: Monad m => ChessUpdate -> StateT ChessState m ()
	updateBoard = modify . applyUpdate

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
					guard (spot == (getUpdatedPosition n))
					return n
				getUpdatedPosition (Move _ _ after) = after
				getUpdatedPosition (Eat _ _ _ after) = after
				trace spot ray = do
					let next = addmove spot ray
					guard (withinboard next)
					maybe (next : (trace next ray)) (\color -> if (color == (getColor p)) then [] else [next]) (pieceAt next)
				pieceAt location = fmap getColor (lookup location (map swap cstate))
				(==>) p q = not (p && (not q))
				(<=>) p q = (p ==> q) && (q ==> p)
	
	isInCheck mycolor cstate = [] /= do
		p <- map fst cstate
		guard (getColor p /= mycolor)
		update <- possibleMoves p cstate
		case update of
			Eat _ p' _ _ -> guard (getType p' == King)
			Move _ _ _ -> guard False
		return update

	isInCheckMate mycolor cstate = (isInCheck mycolor cstate) && permanentlyChecked
		where
			permanentlyChecked = [] == do
				p <- map fst cstate
				guard (getColor p == mycolor)
				update <- possibleMoves p cstate
				let cstate' = applyUpdate update cstate
				guard (isInCheck mycolor cstate')
				return update

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
	markovChess p startState = Markov (return startState) $ \cstate -> do
		move <- uniformSpace $ possibleMoves p cstate
		return (applyUpdate move cstate)

	runMarkovChessWalk :: RandomGen r => Int -> Piece -> ChessState -> Distribution r (Int, Int)
	runMarkovChessWalk n p startState = (flip evalStateT) (markovChess p startState) $ do
		replicateM_ n transitionState
		dState <- gets getStateDistribution
		state <- lift dState
		return $ fromJust . lookup p $ state
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

	interactiveChess :: RandomGen r => StateT ChessState (DistributionT r IO) ()
	interactiveChess = forever $ do
		moveAI <- makeMove
		updateBoard moveAI
		printBoard moveAI
		moveHuman <- allowHumanMove
		updateBoard moveHuman

	makeMove :: RandomGen r => StateT ChessState (DistributionT r IO) ChessUpdate
	makeMove = do
		state <- get
		let pieces = map fst state
		piece <- if (isInCheck Black state)
			then return $ head . filter (\p -> (getColor p == Black) && (getType p == King)) $ pieces
			else lift $ (uniformSpace pieces) `given` (\p -> getColor p == Black && possibleMoves p state /= [])
		move <- lift $ uniformSpace $ possibleMoves piece state
		return move

	allowHumanMove :: RandomGen r => StateT ChessState (DistributionT r IO) ChessUpdate
	allowHumanMove = do
		command <- readHumanMove
		let from = read . takeWhile (/= ' ') $ command :: (Int, Int)
		let to = read . tail . dropWhile (/= ' ') $ command :: (Int, Int)
		maybe_p <- gets (lookup from . map swap)
		case maybe_p of
			Just p -> do
				if (getColor p == White)
					then do
						state <- get
						let moves = possibleMoves p state
						case lookup to . map moveAfter $ moves of
							Just move -> do
								return move
							Nothing -> do
								lift . lift $ putStrLn "Not a valid move. Try again."
								allowHumanMove
					else do
						lift . lift $ putStrLn "You are only allowed to move pieces of your color (White). Try again."
						allowHumanMove
			Nothing -> do
				lift . lift $ putStrLn "No piece found at given location. Try again."
				allowHumanMove
			where
				moveAfter (Move p before after) = (after, Move p before after)
				moveAfter (Eat p p' before after) = (after, Eat p p' before after)

	readHumanMove :: RandomGen r => StateT ChessState (DistributionT r IO) String
	readHumanMove = lift . lift $ getLine

	printBoard :: RandomGen r => ChessUpdate -> StateT ChessState (DistributionT r IO) ()
	printBoard move = do
		forM_ [1 .. 8] $ \y -> do
			forM_ [1 .. 8] $ \x -> do
				maybe_p <- gets (lookup (x, y) . map swap)
				case maybe_p of
					Nothing -> lift . lift . putStr $ "   "
					Just p -> do
						let c = case getColor p of
							White -> "w"
							Black -> "b"
						lift . lift . putStr $ " " ++ c ++ case getType p of
							Pawn -> "p"
							Rook -> "r"
							Knight -> "n"
							Bishop -> "b"
							Queen -> "Q"
							King -> "K"
			lift . lift . putStrLn $ ""