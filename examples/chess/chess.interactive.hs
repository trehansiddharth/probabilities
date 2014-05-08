module Chess.Interactive where
	import Prelude hiding (id, (.))
	import Probabilities
	import Probabilities.Markov
	import Chess
	import System.Random
	import Control.Monad
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Control.Category
	import Data.Tuple (swap)

	interactiveChess :: RandomGen r => StateT ChessState (DistributionT r IO) ()
	interactiveChess = forever $ do
		printBoard
		moveHuman <- allowHumanMove
		updateBoard moveHuman
		moveAI <- allowAIMove
		updateBoard moveAI

	allowAIMove :: RandomGen r => StateT ChessState (DistributionT r IO) ChessUpdate
	allowAIMove = do
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

	printBoard :: RandomGen r => StateT ChessState (DistributionT r IO) ()
	printBoard = do
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