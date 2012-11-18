module AL.Compile (
		Database(..),
		compile
	) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, isJust)
import Data.Char (isUpper)
import qualified Data.Map as M
import Data.List (foldl')

import AL.Core
import AL.Parse

-- |The database class stores all the information needed
-- to fetch a query.
data Database = Database {
						dmap :: (M.Map String [[String]])
					}

-- |The compiler function takes a string and converts
-- it into a AL database.
compile :: String -> Maybe Database
compile = liftM compileClause . liftM catMaybes . parseAL


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


parseClause :: [[String]] -> Clause
parseClause = map (\(Just x) -> x) . filter isJust . map (parseC [])


parseC :: [String] -> [String] -> Maybe Rule
parseC _ [] = Nothing
parseC ys (x:xs) = case x of
					"->" -> Just (Relation (last ys) (init ys))
					_ -> parseC (x:ys) xs