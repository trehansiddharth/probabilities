{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities.Markov where
	import Probabilities
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Control.Monad.Identity

	data (RandomGen r, Eq a, Monad m) => MarkovT r m a = Markov { getStateDistribution :: DistributionT r m a, transitionFunction :: a -> DistributionT r m a }

	type Markov r = MarkovT r Identity

	transition :: (RandomGen r, Eq a, Monad m) => MarkovT r m a -> DistributionT r m (MarkovT r m a)
	transition markov = return $ Markov (getStateDistribution markov >>= transitionFunction markov) (transitionFunction markov)

	transitionState :: (RandomGen r, Eq a, Monad m) => StateT (MarkovT r m a) (DistributionT r m) ()
	transitionState = get >>= lift . transition >>= put