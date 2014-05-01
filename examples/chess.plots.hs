{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where
	import Chess
	import Probabilities
	import Diagrams.Prelude
	import Diagrams.Backend.SVG.CmdLine
	import GHC.Float

	main = do
		samples <- sampleIO 10000 $ king (4, 4) 3
		mainWith $ output $ toSpread samples
			where
				toSpread samples = map (map (\x -> (fromIntegral x) / (fromIntegral . length $ samples))) . tally $ samples

	output :: [[Double]] -> Diagram B R2
	output spread = chessboard $ map (map (\s -> if s == 0 then mempty else circle (s * 5) # fc black)) spread

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
			pattern = [square 1.5 # fc white, square 1.5 # fc grey]
			oddrow = take 8 . cycle $ pattern
			evenrow = take 8 . cycle . reverse $ pattern
			board = take 8 . cycle $ [oddrow, evenrow]

	grid :: [[Diagram B R2]] -> Diagram B R2
	grid = column . map row

	row :: [Diagram B R2] -> Diagram B R2
	row ds = foldl (|||) (head ds) (tail ds)

	column :: [Diagram B R2] -> Diagram B R2
	column ds = foldl (===) (head ds) (tail ds)