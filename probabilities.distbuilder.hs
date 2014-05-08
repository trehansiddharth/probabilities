{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities.DistBuilder where
	import Probabilities
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Control.Monad.Identity

	data DistBuilder a = DistBuilder { getNumPoints :: Int, getSpace :: [a] }

	-- TODO: make addValue use a constant number of random seeds, not O(n) seeds
	addValue :: a -> DistBuilder a -> DistBuilder a
	addValue x dBuilder = DistBuilder n $ x : (getSpace dBuilder)
			where
				n = 1 + getNumPoints dBuilder

	addValueState :: Monad m => a -> StateT (DistBuilder a) m ()
	addValueState x = modify (addValue x)

	getDist :: (RandomGen r, Monad m, Eq a) => DistBuilder a -> DistributionT r m a
	getDist = uniformSpace . getSpace

	getDistState :: (RandomGen r, Monad m, Monad m2, Eq a) => StateT (DistBuilder a) m2 (DistributionT r m a)
	getDistState = gets getDist

	newDistBuilder :: DistBuilder a
	newDistBuilder = DistBuilder 0 []