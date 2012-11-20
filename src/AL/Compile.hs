module AL.Compile (
		Database(..),
		compile'
	) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, isJust)
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
compileClause = foldl' evalRule db
	where
		db = Database M.empty


evalRule :: Database -> Rule -> Database
evalRule (Database m) (Relation n xs) = Database $ M.insertWith union n [xs] m
evalRule db _ = db


getEntries :: Database -> String -> [[String]]
getEntries db@(Database m) query = dmap db M.! query


getEntriesMarked :: Database -> [String] -> String -> [[(String, String)]]
getEntriesMarked db@(Database m) vars query = evalVariables vars $ getEntries db query


commonMarkedEntries :: Database -> [String] -> String -> String -> [[(String, String)]]
commonMarkedEntries db vars q1 q2 = intersect es1 es2
	where
		es1 = getEntriesMarked db vars q1
		es2 = getEntriesMarked db vars q2


-- This function provides the desired variable combinations.
evalVariables :: [String] -> [[String]] -> [[(String, String)]]
evalVariables vars rs = filter ((==length vars).length) $ map (go vars) rs
	where
		go [] [] = []
		go (v:vs) (r:rs) = if isUpper (head v) then (v, r):go vs rs
							else if v == r then ("_", r): go vs rs
							else []