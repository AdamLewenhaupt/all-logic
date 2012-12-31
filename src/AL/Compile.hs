{-# LANGUAGE TupleSections #-}

module AL.Compile (
		Database(..)
		, compile'
		, tests
	) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Char (isUpper, isLower)
import qualified Data.Map as M
import Data.List (foldl', union, (\\), intersect, nub, find)
import Data.Foldable (foldr')
import Control.Applicative ((<*>), (<$>))
import Test.HUnit

import AL.Core
import AL.Parse hiding (tests)

import Prelude hiding (Either(..))

-- |The database class stores all the information needed
-- to fetch a query.
data Database = Database {
						dmap :: M.Map String [[String]]
					} deriving (Show)

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
compile' = liftM (foldl' go defDB) . liftM catMaybes . parseAL
	where
		defDB = emptyDB
		go db r = addClause db $ compileClause db r


-- |Adds a clause to a database.
addClause :: Database -> Clause -> Database
addClause db c = case baseRule c of
	(Relation name _) -> go name
	(Imply (Relation name _) r2) -> go name
	where go name = Database $ M.insertWith union name (nub $ catMaybes $ createSets db c) (dmap db)


-- |Takes a base rule and creates a clause from it.
compileClause :: Database -> Rule -> Clause
compileClause db = saturateCandidates db . (flip Clause) M.empty


createSets :: Database -> Clause -> [Maybe [String]]
createSets db c = map (ruleCmp db $ baseRule c) options 
	where
		options :: [[(String, String)]]
		options = filter (\x -> (length . nub . map snd) x == length x) $ createCombinations $ M.toList $ variableMap c


createCombinations :: [(String, [String])] -> [[(String, String)]]
createCombinations [] = []
createCombinations xs = (\(x:xs) -> go (map (\a -> [a]) x) xs) . map (\(a,b) -> map (a,) b) $ xs
	where
		go :: [[(String, String)]] -> [[(String, String)]] -> [[(String, String)]]
		go acc [] = acc
		go acc (x:xs) = go (concat $ map (\a -> map (:a) x) acc) xs


-- |Used to compare rules resulting in a maybe set of variables if
-- the expression is valid.
ruleCmp :: Database -> Rule -> [(String, String)] -> Maybe [String]
ruleCmp db (Relation name vars) vlist = relationCmp db vlist name $ catMaybes $ map (satRelVar vlist) vars 
ruleCmp db (And r1 r2) vlist = (boolToMby . (==2) . length . catMaybes . map (applyCmp db vlist) ) [r1, r2]
ruleCmp db (Or r1 r2) vlist = (boolToMby . not . null . catMaybes . map (applyCmp db vlist) ) [r1, r2]
ruleCmp db (AndNot r1 r2) vlist = (boolToMby . validAndNot . map (applyCmp db vlist) ) [r1, r2] 
ruleCmp db (Imply (Relation _ vars) r2) vlist = if isJust $ applyCmp db vlist r2 then Just $ catMaybes $ map (satRelVar vlist) vars else Nothing
ruleCmp db  _ vlist = Nothing


relationCmp :: Database -> [(String, String)] -> String -> [String] -> Maybe [String]
relationCmp db vlist name x = if any ((/="_") . fst) vlist
							then assertExist db name x
							else Just x


assertExist :: Database -> String -> [String] -> Maybe [String]
assertExist (Database dm) name vars = case find (vars==) <$> M.lookup name dm of
					Just x -> x
					Nothing -> Nothing


-- |Saturate relation with variable
satRelVar vlist x = if isUpper $ head x 
						then lookup x vlist 
						else Just x


-- |Used for recursive comparing
applyCmp :: Database -> [(String, String)] -> Rule -> Maybe [String]
applyCmp db vlist = (flip (ruleCmp db) $ vlist)


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
		go acc (name, vars) = M.unionWith union acc $ createVarMap $ getEntriesMarked db vars name


-- |Analyzes a rule, finding all dynamic variables.
extractVars :: Rule -> [String]
extractVars = extractRule $ filter (isUpper. head) . snd


-- |Higher-order function that takes a rule and draws all the data from it using
-- the passed funtion.
extractRule :: Eq a => ((String, [String]) -> [a]) -> Rule -> [a]
extractRule f (Relation name vars) = f (name, vars)
extractRule f (Or r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f (AndNot r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f (And r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f (Imply r1 r2) = extractRule f r1 `union` extractRule f r2
extractRule f _ = []


emptyDB :: Database
emptyDB = Database M.empty


createVarMap :: [[(String, String)]] -> Variables
createVarMap = foldl' go M.empty
	where
		go acc = M.unionWith union acc . foldl' go' M.empty
		go' acc (name,val) = M.insertWith union name [val] acc


-- |Extends the root wrapper giving back the information with variables marked.
getEntriesMarked :: Database -> [String] -> String -> [[(String, String)]]
getEntriesMarked db@(Database m) vars = evalVariables vars . getEntries db


-- |Root wrapper for getting database information.
getEntries :: Database -> String -> [[String]]
getEntries db@(Database m) query = case M.lookup query $ dmap db of
	Just x -> x
	Nothing -> []


-- This function provides the desired variable combinations.
evalVariables :: [String] -> [[String]] -> [[(String, String)]]
evalVariables vars rs
		| all (isLower . head) vars = staticVarEval vars
		| otherwise  = filter ((==length (head rs)).length) $ map (go vars) rs
	where
		go _ [] = []
		go [] (r:rs) = ("_", r):go [] rs
		go (v:vs) (r:rs) = if isUpper (head v) then (v, r):go vs rs
							else if v == r then ("_", r): go vs rs
							else []



staticVarEval :: [String] -> [[(String, String)]]
staticVarEval xs = [map ("_",) xs]



-- Testing
tests = test [
		"extractVars1" ~: ["X", "Y", "Z"] ~=? extractVars (Imply (Relation "w" ["X"]) (And (Relation "bla" ["X", "Y"]) (Relation "bly" ["Z", "X"])))

		,"createVarMap1" ~: M.fromList [("X", ["bob", "steve"]), ("Y", ["pete"])] ~=? createVarMap _testVarMap

		,"createCombinations" ~: [[("X", "bob")], [("X", "steve")]] ~=? createCombinations [("X", ["bob", "steve"])]
		,"createSets" ~: [Just ["bob"], Just ["steve"]] ~=? createSets _testDB2 (compileClause _testDB2 $ Relation "name" ["X"])
		,"createSets" ~: [Just ["bob", "bacon"]] ~=? createSets _testDB2 (compileClause _testDB2 $ Relation "eat" ["X", "Y"])

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
_testDB = M.fromList [("love", [["romeo", "julia"], ["julia", "romeo"], ["peter", "julia"]])]
_testDB2 = Database $ M.fromList [("name", [["bob"], ["steve"]]), ("eat", [["bob", "bacon"]])]

-- For debugging
debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')