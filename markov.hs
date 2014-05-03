{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities.Markov where
	import Probabilities
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy

	data Eq a => MarkovChain a = State { getCurrentState :: a, getTransitions :: [(MarkovChain a, Float)] }
	
	instance Eq a => Eq (MarkovChain a) where
		(==) x y = getCurrentState x == getCurrentState y

	transition :: (RandomGen r, Eq a) => MarkovChain a -> Distribution r (MarkovChain a)
	transition = choice . getTransitions

	transitionState :: (RandomGen r, Eq a) => StateT (MarkovChain a) (Distribution r) ()
	transitionState = do
		chain <- get
		next <- lift . transition $ chain
		put next

	runMarkov :: (RandomGen r, Eq a) => Int -> MarkovChain a -> Distribution r (MarkovChain a)
	runMarkov 0 chain = return chain
	runMarkov n chain = transition chain >>= runMarkov (n - 1)

	scanMarkov :: (RandomGen r, Eq a) => Int -> MarkovChain a -> Distribution r [a]
	scanMarkov 0 chain = undefined
	scanMarkov n chain = undefined

	exampleMarkov = x
		where
			x = State 0 [(x, 0.8), (y, 0.2)]
			y = State 1 [(x, 0.5), (y, 0.5)]