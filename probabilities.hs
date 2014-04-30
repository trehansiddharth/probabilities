{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities where
	import Prelude
	import System.Random

	data RandomGen r => Distribution r a = Distribution (r -> a)

	draw r (Distribution f) = f r

	drawIO d = do
		g <- newStdGen
		return $ draw g d

	sample 0 d r = []
	sample n d r = (draw r d) : (sample (n-1) d nxt)
		where
			nxt = snd . split $ r

	sampleIO n d = do
		g <- newStdGen
		return $ sample n d g

	instance RandomGen r => Monad (Distribution r) where
		return x = Distribution $ \r -> x
		(>>=) (Distribution d) f = Distribution $ \r -> ((\(Distribution x) -> x) . f . d . fst . split $ r) $ snd . split $ r

	fromICDF :: RandomGen r => (Float -> a) -> Distribution r a
	fromICDF f = uniform 0.0 1.0 >>= return . f

	getCDF :: (a -> Float) -> (a -> Float)
	getCDF = undefined

	getICDF :: (a -> Float) -> (Float -> a)
	getICDF = undefined

	fromPMF :: RandomGen r => (a -> Float) -> Distribution r a
	fromPMF f = fromICDF . getICDF $ f

	uniform :: (RandomGen r, Random a) => a -> a -> Distribution r a
	uniform a b = Distribution $ fst . randomR (a, b)

	delta :: RandomGen r => a -> Distribution r a
	delta = return

	triangle :: (RandomGen r, Random a, Fractional a) => a -> a -> Distribution r a
	triangle a b = convolve (+) d d
		where
			d = uniform (a/2) (b/2)

	convolve f d1 d2 = do
		x <- d1
		y <- d2
		return $ f x y

	joint :: RandomGen r => Distribution r a -> Distribution r b -> Distribution r (a, b)
	joint = convolve (,)

	given :: RandomGen r => Distribution r (a, b) -> (b -> Bool) -> Distribution r a
	given d p = do
		(x, y) <- d
		if (p y) then (return x) else (given d p)

	match p = (=<<) (return . p)

	-- sandbox

	dJoint :: RandomGen r => Distribution r (Float, Float)
	dJoint = do
		x <- uniform 0.0 1.0
		y <- uniform 1.0 3.0
		return (x, x + y)