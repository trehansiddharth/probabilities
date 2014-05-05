{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities.Markov where
	import Probabilities
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy

	data (RandomGen r, Eq a) => Markov r a = State { getCurrentState :: a, transition :: Distribution r (Markov r a) }
	
	instance (RandomGen r, Eq a) => Eq (Markov r a) where
		(==) x y = getCurrentState x == getCurrentState y

	fromTransitionFunction :: (RandomGen r, Eq a) => a -> (a -> Distribution r a) -> Markov r a
	fromTransitionFunction startState f = State startState $ do
		s <- f startState
		return $ fromTransitionFunction s f

	transitionState :: (RandomGen r, Eq a) => StateT (Markov r a) (Distribution r) ()
	transitionState = get >>= lift . transition >>= put