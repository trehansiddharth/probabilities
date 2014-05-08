module WhatWouldISay where
	import Prelude hiding (id, (.))
	import Probabilities
	import Probabilities.Markov
	import Probabilities.DistBuilder
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Control.Monad.Identity
	import Control.Category

	data Lens a b = Lens (a -> b) (a -> b -> a)

	onlyFst = Lens fst (\(x, y) x' -> (x', y))
	onlySnd = Lens snd (\(x, y) y' -> (x, y'))
	newState state = Lens (const state) (\original state' -> original)
	newFst state = Lens (\original -> (state, original)) (\original (state', original') -> original')
	newSnd state = Lens (\original -> (original, state)) (\original (original', state') -> original')

	instance Category Lens where
		id = Lens id const
		(.) (Lens split1 merge1) (Lens split2 merge2) = Lens (split1 . split2) (\state part -> merge2 state (merge1 (split2 state) part))

	with :: Monad m => Lens s s' -> StateT s' m a -> StateT s m a
	with (Lens split merge) f = do
		state <- get
		result <- lift $ runStateT f (split state)
		put (merge state (snd result))
		return (fst result)

	firstTwo :: [a] -> Maybe (a, a)
	firstTwo [] = Nothing
	firstTwo [x] = Nothing
	firstTwo (x:y:xs) = Just (x, y)

	runWhatWouldISay :: String -> IO ()
	runWhatWouldISay corpus = do
		g <- newStdGen
		let input = concat . map words . lines $ corpus
		runDistT (runStateT whatWouldISay input) g
		return ()

	whatWouldISay :: RandomGen r => StateT [String] (DistributionT r IO) ()
	whatWouldISay = do
		dBigrams <- with (newSnd newDistBuilder) buildBigrams
		let transitionModel = \s -> dBigrams `givenA` (== s)
		(startState, _) <- lift dBigrams
		let markovModel = Markov (return startState) transitionModel
		with (newState markovModel) say

	buildBigrams :: RandomGen r => StateT ([String], DistBuilder (String, String)) (DistributionT r IO) (DistributionT r IO (String, String))
	buildBigrams = do
		maybePair <- with onlyFst $ gets firstTwo
		case maybePair of
			Just pair -> do
				with onlyFst $ modify (drop 1)
				with onlySnd $ addValueState pair
				buildBigrams
			Nothing -> do
				with onlySnd $ getDistState

	say :: RandomGen r => StateT (MarkovT r IO String) (DistributionT r IO) ()
	say = do
		word <- collapseState
		lift . lift . putStr $ word ++ " "
		transitionState
		if elem '.' word then lift . lift . putStrLn $ "" else say