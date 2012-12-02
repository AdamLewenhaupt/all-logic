module AL.Compile (
		Database(..)
		, compile'
		, tests
	) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Char (isUpper)
import qualified Data.Map as M
import Data.List (foldl', union, (\\), intersect, nub)
import Control.Applicative ((<*>), (<$>))
import Test.HUnit

import AL.Core
import AL.Parse hiding (tests)

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
compileCandidates db (Or r1 r2) = union <$> compileCandidates db r1 <*> compileCandidates db r2
compileCandidates db (And r1 r2) = fmap nub $ Just common <*> compileCandidates db r1 <*> compileCandidates db r2
compileCandidates db (AndNot r1 r2) = fmap nub $ Just uncommon <*> compileCandidates db r1 <*> compileCandidates db r2
compileCandidates _ _ = Nothing


uncommon :: [[(String, String)]] -> [[(String, String)]] -> [[(String, String)]]
uncommon xs ys = catMaybes $ cmpCandidatesWith cmpUncommon seed tar
	where
		(seed,tar) = shortest xs ys


-- |The common function takes two lists of candidates and give back all candidates where
-- all variables are equal.
common :: [[(String, String)]] -> [[(String, String)]] -> [[(String, String)]]
common xs ys = catMaybes $ cmpCandidatesWith cmpCommon seed tar
	where
		(seed,tar) = shortest xs ys


-- |A higher order function that takes a comparer and applies it to two lists of candidates.
cmpCandidatesWith :: ([(String, String)] -> [(String, String)] -> Maybe [(String, String)]) -> [[(String, String)]] -> [[(String, String)]] -> [Maybe [(String, String)]]
cmpCandidatesWith _ [] _ = []
cmpCandidatesWith f (a:as) bs = final:cmpCandidatesWith f as bs
	where
		final = if length items > 0 then Just $ head items else Nothing
		items = catMaybes $ map (f a) bs

-- |The cmpCommon function finds all the candidates where all variables are the same.
cmpCommon :: [(String, String)] -> [(String, String)] -> Maybe [(String, String)]
cmpCommon = cmpWith and


cmpUncommon :: [(String, String)] -> [(String, String)] -> Maybe [(String, String)]
cmpUncommon = cmpWith (not . or)


-- |The cmpWith function takes a function that takes a function that take a set of bools
-- and produce a single bool from it where the list of bools are true if there was
-- a variable match found. If the passed function returns true cmpWith returns the element with most variables.
cmpWith :: ([Bool] -> Bool) -> [(String, String)] -> [(String, String)] -> Maybe [(String, String)]
cmpWith f xs ys = if res then Just tar else Nothing
	where
		res = f $ map ($tar) tests
		tests = map elem seed
		cxs = comparable xs
		cys = comparable ys
		(seed,tar) = shortest cxs cys


-- |Makes a list of candidates comparable through
-- ripping away all static values and only keeping variables.
comparable :: [(String, String)] -> [(String, String)]
comparable = filter (isUpper . head . fst)


shortest :: Ord a => [a] -> [a] -> ([a], [a])
shortest xs ys = if length xs <= length ys then (xs,ys) else (ys,xs) 


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


-- Testing
tests = test [
		-- TODO fix bug.
		"cmpUncommon1" ~: Just [("X", "peter"), ("Y", "julia")] ~=? cmpUncommon [("X","peter"), ("Y", "julia")] [("Y", "peter"), ("X", "julia")]

		,"getEntriesMarked1" ~: [[("X", "peter"), ("Y", "julia")], [("X", "julia"), ("Y", "romeo")], [("X", "romeo"), ("Y", "julia")]] 
			~=? getEntriesMarked _tdb2 ["X", "Y"] "love"

		,"uncommon1" ~: [[("X", "peter"), ("Y", "julia")]] ~=? uncommon [[("X", "peter"), ("Y", "julia")]] [[]]
		,"uncommon2" ~: [] ~=? uncommon [[("X", "romeo"), ("Y", "julia")]] [[("Y", "julia"), ("X", "romeo")]]
		,"uncommon3" ~: [[("X", "peter"), ("Y", "julia")]] ~=? uncommon (getEntriesMarked _tdb2 ["X", "Y"] "love") (getEntriesMarked _tdb2 ["Y", "X"] "love")

		,"Relation1" ~: M.fromList [("work", [["bob"]])] ~=? dmap (_ce $ Relation "work" ["bob"])
		, "AndNot1" ~: [["steve"]] ~=?  ((M.! "lame") . dmap) (constructDB _tdb1 $ Imply (Relation "lame" ["X"]) $ AndNot (Relation "name" ["X"]) (Relation "eat" ["X", "bacon"]) )
		, "AndNot2" ~: [["peter"]] ~=? ((M.! "unhappy") . dmap) (constructDB _tdb2 $ Imply (Relation "unhappy" ["X"]) $ AndNot (Relation "love" ["X", "Y"]) (Relation "love" ["Y", "X"]) )
	]

_ce = constructDB (Database M.empty)
_tdb1 = fromJust $ compile' "$name bob;$name steve;$eat bob bacon;"
_tdb2 = fromJust $ compile' "$love romeo julia;$love julia romeo;$love peter julia;"


-- For debugging
debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')