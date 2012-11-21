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
						dmap :: M.Map String [[String]]
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
	(Imply (Relation n xs) r2) -> Database $ M.insertWith union n ( 
		impliedMarkedEntries db xs n $ evalRule db r2) m
	_ -> db


evalRule :: Database -> Rule -> [(String, String)]
evalRule db (Relation n xs) = getEs db xs n
evalRule db (And r1 r2) = commonMarkedEntries (evalRule db r1) (evalRule db r2)


-- 
-- Compiler operations.
--
commonMarkedEntries :: [(String, String)] -> [(String, String)] -> [(String, String)]
commonMarkedEntries = intersect


impliedMarkedEntries :: Database -> [String] -> String -> [(String, String)] -> [[String]]
impliedMarkedEntries db tvars tar svars = reverse $ foldl' go [] tvars
	where
		go :: [[String]] -> String -> [[String]]
		go a b
			| isUpper $ head b = map snd (filter ((==b).fst) svars):a
			| otherwise = [b]:a
--
-- End of compiler operations.
--


getCmpEntries :: Database -> [String] -> [String] -> String -> String -> ([(String, String)], [(String, String)])
getCmpEntries db vars1 vars2 q1 q2 = (es1, es2)
		where
			es1 = getEs db vars1 q1
			es2 = getEs db vars2 q2

getEs :: Database -> [String] -> String -> [(String, String)]
getEs db vars = filter ((`elem`vars) . fst) . concat . getEntriesMarked db vars

getEntriesMarked :: Database -> [String] -> String -> [[(String, String)]]
getEntriesMarked db@(Database m) vars query = evalVariables vars $ getEntries db query


getEntries :: Database -> String -> [[String]]
getEntries db@(Database m) query = dmap db M.! query


-- This function provides the desired variable combinations.
evalVariables :: [String] -> [[String]] -> [[(String, String)]]
evalVariables vars rs = filter ((==length (head rs)).length) $ map (go vars) rs
	where
		go _ [] = []
		go [] (r:rs) = ("_", r):go [] rs
		go (v:vs) (r:rs) = if isUpper (head v) then (v, r):go vs rs
							else if v == r then ("_", r): go vs rs
							else []


combinator :: [a] -> [a] -> [[a]]
combinator first second = go first
	where
		go [] = []
		go (x:xs) = (x:second):go xs


debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')