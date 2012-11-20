module AL.Compile (
		Database(..),
		compile'
	) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Char (isUpper)
import qualified Data.Map as M
import Data.List (foldl', union, (\\), intersect)

import AL.Core
import AL.Parse

-- |The database class stores all the information needed
-- to fetch a query.
data Database = Database {
						dmap :: (M.Map String [[String]])
					}

-- |The compiler function takes a string and converts
-- it into a AL database.
compile' :: String -> Maybe Database
compile' = liftM compileClause . liftM catMaybes . parseAL


compileClause :: Clause -> Database
compileClause = foldl' constructDB db
	where
		db = Database M.empty


constructDB :: Database -> Rule -> Database
constructDB db@(Database m) rule = case rule of
	(Relation n xs) -> Database $ M.insertWith union n [xs] m
	_ -> db


getEntries :: Database -> String -> [[String]]
getEntries db@(Database m) query = dmap db M.! query


getEntriesMarked :: Database -> [String] -> String -> [[(String, String)]]
getEntriesMarked db@(Database m) vars query = evalVariables vars $ getEntries db query


commonMarkedEntries :: Database -> [String] -> String -> String -> [(String, String)]
commonMarkedEntries db vars q1 q2 = intersect es1 es2
	where
		getEs = filter ((`elem`vars) . fst) . concat . getEntriesMarked db vars
		es1 = getEs q1
		es2 = getEs q2


-- This function provides the desired variable combinations.
evalVariables :: [String] -> [[String]] -> [[(String, String)]]
evalVariables vars rs = filter ((==length (head rs)).length) $ map (go vars) rs
	where
		go _ [] = []
		go [] (r:rs) = ("_", r):go [] rs
		go (v:vs) (r:rs) = if isUpper (head v) then (v, r):go vs rs
							else if v == r then ("_", r): go vs rs
							else []


debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')