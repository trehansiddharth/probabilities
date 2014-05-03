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

	uniformSpace :: (RandomGen r, Eq a) => [a] -> Distribution r a
	uniformSpace space = choice (map (\x -> (x, p)) space)
		where
			p = 1 / (fromIntegral . length $ space)

	bernoulli :: (RandomGen r, Eq a) => a -> a -> Float -> Distribution r a
	bernoulli success failure p = fromPMF [success, failure] pmf
		where
			pmf x	| x == success	= p
					| x == failure	= 1 - p

	choice :: (RandomGen r, Eq a) => [(a, Float)] -> Distribution r a
	choice dict = fromPMF (map fst dict) (lookup dict)
		where
			lookup [] x = undefined
			lookup ((k, v):ds) x	| k == x	= v
									| otherwise	= lookup ds x

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

	normal :: (RandomGen r, Random a, Floating a) => a -> a -> Distribution r a
	normal mu sigma = do
		r1 <- uniform 0.0 1.0
		r2 <- uniform 0.0 1.0
		return $ mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2) -- The Box-Muller algorithm for generating a normal random variable

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

	condition :: RandomGen r => Distribution r (b -> a) -> Distribution r b -> Distribution r (a, b)
	condition dab db = do
		ab <- dab
		b <- db
		return $ (ab b, b)

	marginalA :: RandomGen r => Distribution r (a, b) -> Distribution r a
	marginalA = fmap fst

	marginalB :: RandomGen r => Distribution r (a, b) -> Distribution r b
	marginalB = fmap snd

	estimate :: (RandomGen r, Fractional b) => Int -> (a -> b) -> Distribution r a -> Distribution r b
	estimate 1 f d = d >>= return . f
	estimate n f d = do
		x <- d
		e <- estimate (n - 1) f d
		let e' = ((f x) + (fromIntegral $ n - 1) * e) / (fromIntegral n)
		return e'

	probability :: (a -> Bool) -> (a -> Float) -- allows you to do nice things like: estimate 1000 (probability even $ binomial 10 0.4
	probability p = \x -> if p x then 1.0 else 0.0

	mean :: (RandomGen r, Fractional a) => Int -> Distribution r a -> Distribution r a
	mean n = estimate n id

	meanInt :: (RandomGen r, Fractional a) => Int -> Distribution r Int -> Distribution r a
	meanInt n = estimate n fromIntegral

	variance :: (RandomGen r, Fractional a) => Int -> Distribution r a -> Distribution r a
	variance n d = do
		e2 <- estimate n (^2) d
		e <- estimate n id d
		return $ e2 - (e^2)

	varianceInt :: (RandomGen r, Fractional a) => Int -> Distribution r Int -> Distribution r a
	varianceInt n d = do
		e2 <- estimate n ((^2) . fromIntegral) d
		e <- estimate n fromIntegral d
		return $ e2 - (e^2)