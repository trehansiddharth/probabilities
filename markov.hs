{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities.Markov where
	import Probabilities
	import System.Random

	data Eq a => MarkovChain a = State { getCurrentState :: a, getTransitions :: [(MarkovChain a, Float)] }
	
	instance Eq a => Eq (MarkovChain a) where
		(==) x y = getCurrentState x == getCurrentState y

	transition :: (RandomGen r, Eq a) => MarkovChain a -> Distribution r (MarkovChain a)
	transition = choice . getTransitions

	runMarkov :: (RandomGen r, Eq a) => Int -> MarkovChain a -> Distribution r (MarkovChain a)
	runMarkov 0 chain = return chain
	runMarkov n chain = transition chain >>= runMarkov (n - 1)

	exampleMarkov = x
		where
			x = State 0 [(x, 0.8), (y, 0.2)]
			y = State 1 [(x, 0.5), (y, 0.5)]