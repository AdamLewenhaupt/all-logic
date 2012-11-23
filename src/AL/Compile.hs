module AL.Compile (
		Database(..),
		compile'
	) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Char (isUpper)
import qualified Data.Map as M
import Data.List (foldl', union, (\\), intersect)
import Control.Applicative ((<*>))

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


-- |A recursive function that creates a database from a rule clause.
constructDB :: Database -> Rule -> Database
constructDB db@(Database m) r = Database $ case r of 
		(Relation n xs) -> M.insertWith union n [xs] m 
		(Imply (Relation n xs) candidates) -> case (imply db xs candidates) of
			Just entries -> M.insertWith union n entries m
			Nothing -> m



-- |This function generates entries for an implification.
imply :: Database ->  [String] -> Rule -> Maybe [[String]]
imply db template candidates = liftM (merge template) implied
	where
		implied = compileCandidates db candidates


-- |This function merges a template with a set of candidates.
merge :: [String] -> [[(String, String)]] -> [[String]]
merge ts = map catMaybes . map (go ts)
	where
		go [] _ = []
		go (x:xs) i
			| isUpper $ head x = lookup x i:go xs i
			| otherwise = Just x:go xs i


-- |This recursive function takes a database and a rule and try to determine
-- a list of candidates from said rule.
compileCandidates :: Database -> Rule -> Maybe [[(String, String)]]
compileCandidates db (Relation n xs) = Just $ getEntriesMarked db xs n
compileCandidates db (Or r1 r2) = Just union <*> compileCandidates db r1 <*> compileCandidates db r2
compileCandidates db (And r1 r2) = Just common <*> compileCandidates db r1 <*> compileCandidates db r2
compileCandidates _ _ = Nothing

common :: [[(String, String)]] -> [[(String, String)]] -> [[(String, String)]]
common xs ys = catMaybes $ go seed tar
	where
		(seed,tar) = if length xs <= length ys then (xs,ys) else (ys,xs)
		go [] _ = []
		go (a:as) bs = final:go as bs
			where
				final = if length items > 0 then Just $ head items else Nothing
				items = catMaybes $ map (cmp a) bs

cmp :: [(String, String)] -> [(String, String)] -> Maybe [(String, String)]
cmp xs ys = if res then Just tar else Nothing
	where
		res = and $ map ($tar) tests
		tests = map (any.(==)) seed
		cxs = comparable xs
		cys = comparable ys
		(seed,tar) = if length cxs <= length cys then (cxs,cys) else (cys,cxs)


comparable :: [(String, String)] -> [(String, String)]
comparable = filter (isUpper . head . fst)


-- |Extends the root wrapper giving back the information with variables marked.
getEntriesMarked :: Database -> [String] -> String -> [[(String, String)]]
getEntriesMarked db@(Database m) vars query = evalVariables vars $ getEntries db query


-- |Root wrapper for getting database information.
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


-- For debugging
debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')