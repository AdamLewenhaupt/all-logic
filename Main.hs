module Main where

import AL

main :: IO ()
main = do
	d <- readFile "test.txt"
	case compile d of
		Nothing -> return ()
		Just x -> print (query x "unhappy")