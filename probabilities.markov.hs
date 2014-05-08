{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities.Markov where
	import Probabilities
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Control.Monad.Identity

	data (RandomGen r, Eq a, Monad m) => MarkovState r m a = Uncertain { getUncertainState :: DistributionT r m a } | Certain { getCertainState :: a }

	data (RandomGen r, Eq a, Monad m) => MarkovT r m a = Markov { getState :: MarkovState r m a, transitionFunction :: a -> DistributionT r m a }

	type Markov r = MarkovT r Identity

	transition :: (RandomGen r, Eq a, Monad m) => MarkovT r m a -> (MarkovT r m a)
	transition (Markov state f) = case state of
		Uncertain d -> Markov (Uncertain (d >>= f)) f
		Certain s -> Markov (Uncertain (f s)) f

	transitionState :: (RandomGen r, Eq a, Monad m) => StateT (MarkovT r m a) (DistributionT r m) ()
	transitionState = modify transition

	drawState :: (RandomGen r, Eq a, Monad m) => MarkovT r m a -> DistributionT r m a
	drawState (Markov state f) = case state of
		Uncertain d -> d
		Certain s -> return s

	collapse :: (RandomGen r, Eq a, Monad m) => MarkovT r m a -> DistributionT r m (MarkovT r m a)
	collapse (Markov state f) = case state of
		Uncertain d -> do
			s <- d
			return (Markov (Certain s) f)
		Certain s -> return (Markov state f)

	collapseState :: (RandomGen r, Eq a, Monad m) => StateT (MarkovT r m a) (DistributionT r m) a
	collapseState = do
		markov <- get
		markov' <- lift . collapse $ markov
		put markov'
		lift . drawState $ markov