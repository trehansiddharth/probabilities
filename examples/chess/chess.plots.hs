{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where
	import Chess
	import Probabilities
	import Diagrams.Prelude
	import Diagrams.Backend.SVG.CmdLine
	import GHC.Float
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy

	main = produceChessWalk

	produceChessWalk :: IO ()
	produceChessWalk = do
		samples <- sampleIO 10000 $ runStateT
			(randomChessWalk (Piece Bishop Black 0 True) 2)
			[(Piece Bishop Black 0 True, (1, 1)),
				(Piece Knight White 1 True, (5, 5))]
		mainWith $ bubblePlot $ toSpread . map fst $ samples

	toSpread :: Fractional a => [(Int, Int)] -> [[a]]
	toSpread samples = map (map (\x -> (fromIntegral x) / (fromIntegral . length $ samples))) . tally $ samples

	bubblePlot :: [[Double]] -> Diagram B R2
	bubblePlot spread = chessboard $ map (map (\s -> if s == 0 then mempty else circle (s * k) # fc black)) spread
		where
			k = 0.45 / (maximum (map maximum spread))

	tally :: [(Int, Int)] -> [[Int]]
	tally xs = go (take 8 . repeat . take 8 . repeat $ 0) xs
		where
			go tal [] = tal
			go tal ((x, y):rs) = go (apply y (apply x (+1)) tal) rs

			apply 1 f (x:xs) = (f x):xs
			apply n f (x:xs) = x : (apply (n - 1) f xs)

	chessboard :: [[Diagram B R2]] -> Diagram B R2
	chessboard dss = grid $ zipWith (zipWith (atop)) dss board
		where
			pattern = [square 1.5 # fc antiquewhite, square 1.5 # fc saddlebrown]
			oddrow = take 8 . cycle $ pattern
			evenrow = take 8 . cycle . reverse $ pattern
			board = take 8 . cycle $ [oddrow, evenrow]

	grid :: [[Diagram B R2]] -> Diagram B R2
	grid = column . map row

	row :: [Diagram B R2] -> Diagram B R2
	row ds = foldl (|||) (head ds) (tail ds)

	column :: [Diagram B R2] -> Diagram B R2
	column ds = foldl (===) (head ds) (tail ds)