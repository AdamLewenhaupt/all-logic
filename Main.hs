module Main where

import ALogic

main :: IO ()
main = do
	let items = ["jealous", "happy", "unhappy"]
	d <- readFile "test.txt"
	case compile d of
		Nothing -> return ()
		Just x -> mapM_ putStrLn $ map (\a -> ((a++": ")++) $ show $ query x a) items