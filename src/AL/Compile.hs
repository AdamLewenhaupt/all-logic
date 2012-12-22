{-# LANGUAGE TupleSections #-}

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

import Prelude hiding (Either(..))

-- |The database class stores all the information needed
-- to fetch a query.
data Database = Database {
						dmap :: M.Map String [[String]]
					}

type Variables = M.Map String [String]

data Cmp = Left | Right | Both

data Clause = Clause {
		baseRule :: Rule, 
		variableMap :: Variables
	}
	deriving(Show)


-- |The compiler function takes a string and converts
-- it into a AL database if the parse is successfull else Nothing.
compile' :: String -> Maybe Database
compile' = liftM (foldl' addClause db) . liftM (map $ compileClause db) . liftM catMaybes . parseAL
	where
		db = emptyDB


-- |Adds a clause to a database.
addClause :: Database -> Clause -> Database
addClause db c = Database $ case baseRule c of
	(Relation name vars) -> M.insertWith union name (catMaybes $ createSets db c) (dmap db)
	(Imply (Relation name _) r2) -> M.insertWith union name (catMaybes $ createSets db $ compileClause db r2) (dmap db)

-- |Takes a base rule and creates a clause from it.
compileClause :: Database -> Rule -> Clause
compileClause db = saturateCandidates db . (flip Clause) M.empty


createSets :: Database -> Clause -> [Maybe [String]]
createSets db c = map (ruleCmp $ baseRule c) options 
	where
		options :: [[(String, String)]]
		options = createCombinations $ M.toList $ variableMap c


createCombinations :: [(String, [String])] -> [[(String, String)]]
createCombinations = (\(x:xs) -> go (map (\a -> [a]) x) xs) . map (\(a,b) -> map (a,) b)
	where
		go :: [[(String, String)]] -> [[(String, String)]] -> [[(String, String)]]
		go acc [] = acc
		go acc (x:xs) = go (concat $ map (\a -> map (:a) x) acc) xs


-- |Used to compare rules resulting in a maybe set of variables if
-- the expression is valid.
ruleCmp :: Rule -> [(String, String)] -> Maybe [String]
ruleCmp (Relation name vars) vlist = Just $ catMaybes $ map (satRelVar vlist) vars
ruleCmp (And r1 r2) vlist = (boolToMby . (==2) . length . catMaybes . map (applyCmp vlist) ) [r1, r2]
ruleCmp (Or r1 r2) vlist = (boolToMby . not . null . catMaybes . map (applyCmp vlist) ) [r1, r2]
ruleCmp (AndNot r1 r2) vlist = (boolToMby . validAndNot . map (applyCmp vlist) ) [r1, r2] 
ruleCmp (Imply r1 r2) vlist = if isJust $ applyCmp vlist r2 then ruleCmp r1 vlist else Nothing
ruleCmp  _ vlist = Nothing


-- |Saturate relation with variable
satRelVar vlist x = if isUpper $ head x 
						then lookup x vlist 
						else Just x


-- |Used for recursive comparing
applyCmp :: [(String, String)] -> Rule -> Maybe [String]
applyCmp vlist = (flip ruleCmp $ vlist)


-- |Check out if a AndNot expression is valid.
validAndNot :: [Maybe [a]] -> Bool
validAndNot [Just _, Nothing] = True
validAndNot _ = False


-- |Takes a bool and converts it to an empty list if true else Nothing.
boolToMby :: Bool -> Maybe [String]
boolToMby x = if x then Just [] else Nothing


-- |Transform a clause, saturationg all the variable combinations.
saturateCandidates :: Database -> Clause -> Clause
saturateCandidates db clause = Clause base $ foldl' go varMap $ extractRule (\x -> [x]) $ base
	where
		base = baseRule clause
		varMap = variableMap clause
		varSet = extractVars $ base
		go acc (name, vars) = M.union acc $ createVarMap $ case getEntriesMarked db vars name of
			Just x -> x
			Nothing -> []


-- |Analyzes a rule, finding all dynamic variables.
extractVars :: Rule -> [String]
extractVars = extractRule $ filter (isUpper. head) . snd


-- |Higher-order function that takes a rule and draws all the data from it using
-- the passed funtion.
extractRule :: Eq a => ((String, [String]) -> [a]) -> Rule -> [a]
extractRule f (Relation name vars) = f (name, vars)
extractRule f (AndNot r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f (And r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f (Imply r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f _ = []


emptyDB :: Database
emptyDB = Database M.empty


createVarMap :: [[(String, String)]] -> Variables
createVarMap = foldl' go M.empty
	where
		go acc = M.unionWith (++) acc . foldl' go' M.empty
		go' acc (name,val) = M.insertWith union name [val] acc


-- |Extends the root wrapper giving back the information with variables marked.
getEntriesMarked :: Database -> [String] -> String -> Maybe [[(String, String)]]
getEntriesMarked db@(Database m) vars = fmap (evalVariables vars) . getEntries db


-- |Root wrapper for getting database information.
getEntries :: Database -> String -> Maybe [[String]]
getEntries db@(Database m) query = M.lookup query $ dmap db


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

		,"createVarMap1" ~: M.fromList [("X", ["bob", "steve"]), ("Y", ["pete"])] ~=? createVarMap _testVarMap

		,"ruleCmp1" ~: Just ["bob", "peter"] ~=? ruleCmp _testRule1 [("X", "bob"), ("Y", "peter"), ("Z", "sello")]
		,"ruleCmp2" ~: Just ["cat", "fish"] ~=? ruleCmp _testRule2 [("X", "cat"), ("Y", "fish"), ("Z", "dog")]           

		,"Relation1" ~: M.fromList [("work", [["bob"]])] ~=? dmap ((_ce . compileClause emptyDB) $ Relation "work" ["bob"])
		, "AndNot1" ~: [["steve"]] ~=?  ((M.! "lame") . dmap) (addClause _testDB2 $ compileClause _tdb1 $ Imply (Relation "lame" ["X"]) $ AndNot (Relation "name" ["X"]) (Relation "eat" ["X", "bacon"]) )
		, "AndNot2" ~: [["peter"]] ~=? ((M.! "unhappy") . dmap) (addClause _tdb2 $ compileClause _tdb2 $ Imply (Relation "unhappy" ["X"]) $ AndNot (Relation "love" ["X", "Y"]) (Relation "love" ["Y", "X"]) )
	]

_testRule2 = (Imply (Relation "eat" ["X", "Y"]) (And (Relation "food" ["X"]) (Relation "hunter" ["Y"]) ) )
_testRule1 = (Relation "work" ["X", "Y"])

_ce = addClause (Database M.empty)
_tdb1 = fromJust $ compile' "$name bob;$name steve;$eat bob bacon;"
_tdb2 = fromJust $ compile' "$love romeo julia;$love julia romeo;$love peter julia;"
_testVarMap = [[("X", "bob"), ("Y", "pete")], [("X", "steve")]]
_testDB = M.fromList [("love", [["romeo", "julia"], ["julia romeo"], ["peter", "julia"]])]
_testDB2 = Database $ M.fromList [("name", [["bob"], ["steve"]]), ("eat", [["bob", "bacon"]])]

-- For debugging
debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')