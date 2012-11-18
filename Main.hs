module Main where

import AL

main :: IO ()
main = do
	d <- readFile "test.txt"
	print $ case compile d of
		Nothing -> []
		Just x -> query x "working"