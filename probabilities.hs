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

	data BST a = Branch { totalProbability :: Float, leftBST :: BST a, rightBST :: BST a } | Leaf { leafProbability :: Float, leafValue :: a }

	probabilityBST :: BST a -> Float
	probabilityBST (Leaf p x) = p
	probabilityBST (Branch p x y) = p

	insertBST :: (RandomGen r, Monad m) => Float -> a -> BST a -> DistributionT r m (BST a)
	insertBST px x (Leaf py y) = return $ Branch (px + py) (Leaf px x) (Leaf py y)
	insertBST px x (Branch p y z) = do
		goleft <- bernoulli True False 0.5
		if goleft
			then do
				left <- insertBST px x y
				return (Branch (p + px) left z)
			else do
				right <- insertBST px x z
				return (Branch (p + px) y right)
			where
				py = probabilityBST y
				pz = probabilityBST z

	makeBST :: (RandomGen r, Monad m) => [(a, Float)] -> DistributionT r m (BST a)
	makeBST ((a, p):dict) = go (Leaf p a) dict
		where
			go bst [] = return bst
			go bst ((b, q):xs) = do
				bst' <- insertBST q b bst
				go bst' xs

	searchBST :: Float -> BST a -> a
	searchBST p (Leaf q x) = x
	searchBST p (Branch q x y) = if (p < px) then (searchBST p x) else (searchBST (p - px) y)
		where
			px = probabilityBST x
			py = probabilityBST y

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
	uniformSpace space = do
		n <- uniform 0 (length space - 1)
		return (space !! n)

	bernoulli :: (RandomGen r, Eq a, Monad m) => a -> a -> Float -> DistributionT r m a
	bernoulli success failure p = do
		x <- uniform 0.0 1.0
		return $ if x < p then success else failure

	choice :: (RandomGen r, Eq a, Monad m) => [(a, Float)] -> DistributionT r m a
	choice dict = do
		bst <- makeBST dict
		p <- uniform 0.0 1.0
		return $ searchBST p bst

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
		if (p x) then (return x) else (d `given` p)

	givenA :: (RandomGen r, Monad m) => DistributionT r m (a, b) -> (a -> Bool) -> DistributionT r m b
	givenA d p = marginalB (d `given` (p . fst))

	givenB :: (RandomGen r, Monad m) => DistributionT r m (a, b) -> (b -> Bool) -> DistributionT r m a
	givenB d p = marginalA (d `given` (p . snd))

	(>>~) :: (RandomGen r, Monad m) => DistributionT r m b -> (b -> DistributionT r m a) -> DistributionT r m (a, b)
	(>>~) db f = do
		b <- db
		a <- f b
		return $ (a, b)

	bayesianUpdate :: (RandomGen r, Eq o, Monad m) => DistributionT r m s -> (s -> DistributionT r m o) -> o -> DistributionT r m s
	bayesianUpdate ds f o = (ds >>~ f) `givenA` (== o)

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

	probability :: (a -> Bool) -> (a -> Float) -- allows you to do nice things like: estimate 1000 (probability even) $ binomial 10 0.4
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