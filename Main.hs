{-
More advanced example
band bob;
band tom;
band pete;

sit bob;

sit X -> drummer X;
band X -> love X groupie;

love X groupie and drummer X?
bob
-}

module Main where

import Data.Char(isUpper)
import Data.List(foldl')
import qualified Data.Map as M

data Database = Database {
						dmap :: (M.Map String [[String]])
					}

data Rule = Relation String [String] | Imply String [String] String [String]
	deriving(Show)
type Clause = [Rule]

main :: IO ()
main = do
	let c = emptyClause
	print $ query [
			(Relation "band" ["bob"]),
			(Relation "band" ["tom"]),
			(Relation "band" ["pete"]),
			(Relation "sit" ["bob"]),
			(Imply "sit" ["X"] "drummer" ["X"]),
			(Imply "band" ["X"] "love" ["X", "groupie"])
		] "love"

query :: Clause -> String -> [[String]]
query c s = ((M.! s) . dmap . compileClause) c

emptyClause :: Clause
emptyClause = []

compileClause :: Clause -> Database
compileClause = foldl' evalRule db
	where
		db = Database M.empty

evalRule :: Database -> Rule -> Database
evalRule (Database m) r = Database (insertion m)
				where 
					insertion = case r of
						(Relation tar vals) -> M.insertWith (++) tar [vals]
						(Imply tar ts val vs) -> M.insertWith (++) val $ 
							implifications (evalVariables ts (m M.! tar)) vs


-- This function provides the desired variable combinations.
evalVariables :: [String] -> [[String]] -> [[(String, String)]]
evalVariables vars rs = filter ((==length vars).length) $ map (go vars) rs
	where
		go [] [] = []
		go (v:vs) (r:rs) = if isUpper (head v)
								then (v, r):go vs rs
							else if v == r 
								then ("_", r): go vs rs
								else []

intersections :: [[(String, String)]] -> [[(String, String)]] -> [[String]]
intersections [] _ = []
intersections _ [] = []
intersections (xs:xss) (ys:yss) = [v1 | (n1, v1) <- xs, (n2, v2) <- ys, n1 == n2]:intersections xss yss

-- Calculates the set after implification.
implifications :: [[(String, String)]] -> [String] -> [[String]]
implifications [] _ = []
implifications (x:xs) ys = map go ys:implifications xs ys
					where
						go y 
							| isUpper $head y = match y x
							| otherwise = y
						match t [] = t
						match t ((n,v):ls) = if n == t then v else match t ls