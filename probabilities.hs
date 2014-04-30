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

	instance RandomGen r => Functor (Distribution r) where
		fmap f d = d >>= return . f

	fromICDF :: RandomGen r => (Float -> a) -> Distribution r a
	fromICDF f = uniform 0.0 1.0 >>= return . f

	getCDF :: (a -> Float) -> (a -> Float)
	getCDF = undefined

	getICDF :: (a -> Float) -> (Float -> a)
	getICDF = undefined

	fromPMF :: (RandomGen r) => [a] -> (a -> Float) -> Distribution r a
	fromPMF space = fromICDF . getICMF space

	getICMF :: [a] -> (a -> Float) -> (Float -> a)
	getICMF space pmf = icmf space
		where
			icmf [x] y = x
			icmf (x:xs) y	| y > pmf x	= icmf xs (y - (pmf x))
							| otherwise	= x

	uniform :: (RandomGen r, Random a) => a -> a -> Distribution r a
	uniform a b = Distribution $ fst . randomR (a, b)

	bernoulli :: (RandomGen r, Eq a) => a -> a -> Float -> Distribution r a
	bernoulli success failure p = fromPMF [success, failure] pmf
		where
			pmf x	| x == success	= p
					| x == failure	= 1 - p

	geometric :: RandomGen r => Float -> Distribution r Int
	geometric p = do
		outcome <- bernoulli True False p
		case outcome of
			True -> return 0
			False -> do
				remaining <- geometric p
				return $ 1 + remaining

	binomial :: RandomGen r => Int -> Float -> Distribution r Int
	binomial 0 p = return 0
	binomial n p = do
		outcome <- bernoulli 1 0 p
		remaining <- binomial (n - 1) p
		return $ outcome + remaining

	randomWalk :: RandomGen r => Int -> Float -> Distribution r Int
	randomWalk 0 p = return 0
	randomWalk n p = do
		step <- bernoulli 1 (-1) p
		remaining <- randomWalk (n - 1) p
		return $ step + remaining

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

	given :: RandomGen r => Distribution r a -> (a -> Bool) -> Distribution r a
	given d p = do
		x <- d
		if (p x) then (return x) else (given d p)

	givenJoint :: RandomGen r => Distribution r (a, b) -> (b -> Bool) -> Distribution r a
	givenJoint d p = given d (\(x, y) -> p y) >>= return . fst

	-- sandbox

	dJoint :: RandomGen r => Distribution r (Float, Float)
	dJoint = do
		x <- uniform 0.0 1.0
		y <- uniform 0.0 1.0
		return (x, x + y)