{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Probabilities where
	import Prelude
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.Class
	import Control.Monad.Identity

	data (RandomGen r, Monad m) => DistributionT r m a = Distribution { runDistT :: r -> m a }

	type Distribution r = DistributionT r Identity

	runDist d = runIdentity . runDistT d

	draw r d = runDist d r

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

	instance (RandomGen r, Monad m) => Monad (DistributionT r m) where
		return x = Distribution $ \r -> return x
		(>>=) d f = Distribution $ \r -> do
			let r1 = fst . split $ r
			let r2 = snd . split $ r
			x <- runDistT d r1
			runDistT (f x) r2

	instance (RandomGen r, Monad m) => Functor (DistributionT r m) where
		fmap f d = d >>= return . f

	instance RandomGen r => MonadTrans (DistributionT r) where
		lift u = Distribution $ \r -> u

	fromICDF :: (RandomGen r, Monad m) => (Float -> a) -> DistributionT r m a
	fromICDF f = uniform 0.0 1.0 >>= return . f

	getCDF :: (a -> Float) -> (a -> Float)
	getCDF = undefined

	getICDF :: (a -> Float) -> (Float -> a)
	getICDF = undefined

	fromPMF :: (RandomGen r, Monad m) => [a] -> (a -> Float) -> DistributionT r m a
	fromPMF space = fromICDF . getICMF space

	getICMF :: [a] -> (a -> Float) -> (Float -> a)
	getICMF space pmf = icmf space
		where
			icmf [x] y = x
			icmf (x:xs) y	| y > pmf x	= icmf xs (y - (pmf x))
							| otherwise	= x

	uniform :: (RandomGen r, Random a, Monad m) => a -> a -> DistributionT r m a
	uniform a b = Distribution $ return . fst . randomR (a, b)

	uniformSpace :: (RandomGen r, Eq a, Monad m) => [a] -> DistributionT r m a
	uniformSpace space = choice (map (\x -> (x, p)) space)
		where
			p = 1 / (fromIntegral . length $ space)

	bernoulli :: (RandomGen r, Eq a, Monad m) => a -> a -> Float -> DistributionT r m a
	bernoulli success failure p = fromPMF [success, failure] pmf
		where
			pmf x	| x == success	= p
					| x == failure	= 1 - p

	choice :: (RandomGen r, Eq a, Monad m) => [(a, Float)] -> DistributionT r m a
	choice dict = fromPMF (map fst dict) (lookup dict)
		where
			lookup [] x = undefined
			lookup ((k, v):ds) x	| k == x	= v
									| otherwise	= lookup ds x

	geometric :: (RandomGen r, Monad m) => Float -> DistributionT r m Int
	geometric p = do
		outcome <- bernoulli True False p
		case outcome of
			True -> return 0
			False -> do
				remaining <- geometric p
				return $ 1 + remaining

	binomial :: (RandomGen r, Monad m) => Int -> Float -> DistributionT r m Int
	binomial 0 p = return 0
	binomial n p = do
		outcome <- bernoulli 1 0 p
		remaining <- binomial (n - 1) p
		return $ outcome + remaining

	randomWalk :: (RandomGen r, Monad m) => Int -> Float -> DistributionT r m Int
	randomWalk 0 p = return 0
	randomWalk n p = do
		step <- bernoulli 1 (-1) p
		remaining <- randomWalk (n - 1) p
		return $ step + remaining

	delta :: (RandomGen r, Monad m) => a -> DistributionT r m a
	delta = return

	triangle :: (RandomGen r, Random a, Fractional a, Monad m) => a -> a -> DistributionT r m a
	triangle a b = convolve (+) d d
		where
			d = uniform (a/2) (b/2)

	normal :: (RandomGen r, Random a, Floating a, Monad m) => a -> a -> DistributionT r m a
	normal mu sigma = do
		r1 <- uniform 0.0 1.0
		r2 <- uniform 0.0 1.0
		return $ mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2) -- The Box-Muller algorithm for generating a normal random variable

	convolve f d1 d2 = do
		x <- d1
		y <- d2
		return $ f x y

	joint :: (RandomGen r, Monad m) => DistributionT r m a -> DistributionT r m b -> DistributionT r m (a, b)
	joint = convolve (,)

	given :: (RandomGen r, Monad m) => DistributionT r m a -> (a -> Bool) -> DistributionT r m a
	given d p = do
		x <- d
		if (p x) then (return x) else (given d p)

	givenJoint :: (RandomGen r, Monad m) => DistributionT r m (a, b) -> (b -> Bool) -> DistributionT r m a
	givenJoint d p = given d (\(x, y) -> p y) >>= return . fst

	($~) :: (RandomGen r, Monad m) => (b -> DistributionT r m a) -> DistributionT r m b -> DistributionT r m (a, b)
	($~) f db = do
		b <- db
		a <- f b
		return $ (a, b)

	observing :: (RandomGen r, Monad m) => DistributionT r m (a, b) -> (a -> Bool) -> DistributionT r m b
	observing dab p = fmap snd $ dab `given` (p . fst)

	bayesianUpdate :: (RandomGen r, Eq o, Monad m) => (s -> DistributionT r m o) -> DistributionT r m s -> o -> DistributionT r m s
	bayesianUpdate f ds o = (f $~ ds) `observing` (== o)

	marginalA :: (RandomGen r, Monad m) => DistributionT r m (a, b) -> DistributionT r m a
	marginalA = fmap fst

	marginalB :: (RandomGen r, Monad m) => DistributionT r m (a, b) -> DistributionT r m b
	marginalB = fmap snd

	estimate :: (RandomGen r, Fractional b, Monad m) => Int -> (a -> b) -> DistributionT r m a -> DistributionT r m b
	estimate 1 f d = d >>= return . f
	estimate n f d = do
		x <- d
		e <- estimate (n - 1) f d
		let e' = ((f x) + (fromIntegral $ n - 1) * e) / (fromIntegral n)
		return e'

	probability :: (a -> Bool) -> (a -> Float) -- allows you to do nice things like: estimate 1000 (probability even $ binomial 10 0.4
	probability p = \x -> if p x then 1.0 else 0.0

	mean :: (RandomGen r, Fractional a, Monad m) => Int -> DistributionT r m a -> DistributionT r m a
	mean n = estimate n id

	meanInt :: (RandomGen r, Fractional a, Monad m) => Int -> DistributionT r m Int -> DistributionT r m a
	meanInt n = estimate n fromIntegral

	variance :: (RandomGen r, Fractional a, Monad m) => Int -> DistributionT r m a -> DistributionT r m a
	variance n d = do
		e2 <- estimate n (^2) d
		e <- estimate n id d
		return $ e2 - (e^2)

	varianceInt :: (RandomGen r, Fractional a, Monad m) => Int -> DistributionT r m Int -> DistributionT r m a
	varianceInt n d = do
		e2 <- estimate n ((^2) . fromIntegral) d
		e <- estimate n fromIntegral d
		return $ e2 - (e^2)