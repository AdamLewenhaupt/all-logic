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

type Variables = M.Map String [String]

data Clause = Clause {
		baseRule :: Rule, 
		variableMap :: Variables
	}


-- |The compiler function takes a string and converts
-- it into a AL database if the parse is successfull else Nothing.
compile' :: String -> Maybe Database
compile' = liftM (foldl' addClause emptyDB) . liftM (map compileClause) . liftM catMaybes . parseAL


-- |Takes a base rule and creates a clause from it.
compileClause :: Rule -> Clause
compileClause r = undefined


-- |Transform a clause, saturationg all the variable combinations.
saturateCandidates :: Database -> Clause -> Clause
saturateCandidates db clause = Clause base $ foldl' go varMap $ extractRule (\x -> [x]) $ base
	where
		base = baseRule clause
		varMap = variableMap clause
		varSet = extractVars $ base
		go acc (name, vars) = M.insertWith go getEntriesMarked db vars name


-- |Analyzes a rule, finding all dynamic variables.
extractVars :: Rule -> [String]
extractVars = extractRule $ filter (isUpper. head) . snd


-- |Higher-order function that takes a rule and draws all the data from it.
extractRule :: Eq a => ((String, [String]) -> [a]) -> Rule -> [a]
extractRule f (Relation name vars) = f (name, vars)
extractRule f (AndNot r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f (And r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f (Imply r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f _ = []


-- |Adds a clause to a database.
addClause :: Database -> Clause -> Database
addClause = undefined

emptyDB :: Database
emptyDB = Database M.empty


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
		"extractVars1" ~: ["X", "Y", "Z"] ~=? extractVars (Imply (Relation "w" ["X"]) (And (Relation "bla" ["X", "Y"]) (Relation "bly" ["Z", "X"])))

		,"Relation1" ~: M.fromList [("work", [["bob"]])] ~=? dmap ((_ce . compileClause) $ Relation "work" ["bob"])
		, "AndNot1" ~: [["steve"]] ~=?  ((M.! "lame") . dmap) (addClause _tdb1 $ compileClause $ Imply (Relation "lame" ["X"]) $ AndNot (Relation "name" ["X"]) (Relation "eat" ["X", "bacon"]) )
		, "AndNot2" ~: [["peter"]] ~=? ((M.! "unhappy") . dmap) (addClause _tdb2 $ compileClause $ Imply (Relation "unhappy" ["X"]) $ AndNot (Relation "love" ["X", "Y"]) (Relation "love" ["Y", "X"]) )
	]

_ce = addClause (Database M.empty)
_tdb1 = fromJust $ compile' "$name bob;$name steve;$eat bob bacon;"
_tdb2 = fromJust $ compile' "$love romeo julia;$love julia romeo;$love peter julia;"


-- For debugging
debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')