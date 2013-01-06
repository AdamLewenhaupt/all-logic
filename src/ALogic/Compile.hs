{-# LANGUAGE TupleSections #-}

module ALogic.Compile (
		Database(..)
		, compile'
		, createDB'
		, tests
	) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Char (isUpper, isLower)
import qualified Data.Map as M
import Data.List (foldl', union)
import Control.Applicative ((<$>))
import qualified Data.Vector as V
import Test.HUnit

import ALogic.Core
import ALogic.Parse hiding (tests)

type Vector a = V.Vector a
type Vector2 a = Vector (Vector a)

-- |The database class stores all the information needed
-- to fetch a query.
data Database = Database {
						dmap :: M.Map String (Vector2 String)
					} deriving (Show)

type Variables = M.Map String (Vector String)

data Cmp = Left | Right | Both

data Clause = Clause {
		baseRule :: Rule, 
		variableMap :: Variables
	}
	deriving(Show)


-- |The compiler function takes a string and converts
-- it into a AL database if the parse is successfull else Nothing.
compile' :: String -> Maybe Database
compile' = liftM createDB' . liftM catMaybes . parseAL


createDB' :: [Rule] -> Database
createDB' = foldl' go emptyDB
	where
		go db r = addClause db $ compileClause db r


-- |Adds a clause to a database.
addClause :: Database -> Clause -> Database
addClause db c = case baseRule c of
	(Relation name _) -> go name
	(Imply (Relation name _) r2) -> go name
	where go name = Database $ M.insertWith vunion name (vnub $ vcatMaybes $ createSets db c) (dmap db)


-- |Takes a base rule and creates a clause from it.
compileClause :: Database -> Rule -> Clause
compileClause db = saturateCandidates db . (flip Clause) M.empty


-- | The createSets function uses the information that is gathered by
-- the createCombinations function and applies ruleCmp to every set
-- in order to create sets that can be inserted into the database.
createSets :: Database -> Clause -> Vector (Maybe (Vector String))
createSets db c = V.map (ruleCmp db $ baseRule c) options 
	where
		options = V.filter (\x -> (V.length . vnub . V.map snd) x == V.length x) $ createCombinations $ M.toList $ variableMap c


-- | This function takes a database in list format and converts it into all
-- variable combinations and then pass it on as a 2D vector with (name, val)
-- relations.
createCombinations :: [(String, Vector String)] -> Vector2 (String, String)
createCombinations [] = V.empty
createCombinations xs = initializator . map (uncurry combinator) $ xs
	where
		go acc [] = acc
		go acc (x:xs) = go (V.concat . V.toList $ V.map (\a -> V.map (`V.cons`a) x) acc) xs
		combinator a = V.map (a,)
		initializator (x:xs) = go (V.map return x) xs


-- |Used to compare rules resulting in a maybe set of variables if
-- the expression is valid.
ruleCmp :: Database -> Rule -> Vector (String, String) -> Maybe (Vector String)
ruleCmp db (Relation name vars) vlist = relationCmp db vlist name $ vcatMaybes $ V.map (satRelVar vlist) $ V.fromList vars 
ruleCmp db (And r1 r2) vlist = (liftM V.fromList . boolToMby . (==2) . length . catMaybes . map (applyCmp db vlist) ) [r1, r2]
ruleCmp db (Or r1 r2) vlist = (liftM V.fromList . boolToMby . not . null . catMaybes . map (applyCmp db vlist) ) [r1, r2]
ruleCmp db (AndNot r1 r2) vlist = (liftM V.fromList . boolToMby . validAndNot . map (applyCmp db vlist) ) [r1, r2] 
ruleCmp db (Imply (Relation _ vars) r2) vlist = if isJust $ applyCmp db vlist r2 then Just . V.fromList $ catMaybes $ map (satRelVar vlist) vars else Nothing
ruleCmp db  _ vlist = Nothing


-- | Helper function for Relation compareing.
relationCmp :: Database -> Vector (String, String) -> String -> Vector String -> Maybe (Vector String)
relationCmp db vlist name x = if V.any ((/="_") . fst) vlist
							then assertExist db name x
							else Just x


-- | Returns Just x, given that x is defined in the database else Nothing.
assertExist :: Database -> String -> Vector String -> Maybe (Vector String)
assertExist (Database db) name vars = case V.find (vars==) <$> M.lookup name db of
					Just x -> x
					Nothing -> Nothing


-- |Saturate relation with variable
satRelVar vlist x = if isUpper $ head x 
						then vlookup x vlist 
						else Just x


-- |Used for recursive comparing
applyCmp :: Database -> Vector (String, String) -> Rule -> Maybe (Vector String)
applyCmp db vlist = (flip (ruleCmp db) $ vlist)


-- |Check out if a AndNot expression is valid.
validAndNot :: [Maybe (Vector a)] -> Bool
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
		go acc (name, vars) = M.unionWith vunion acc $ createVarMap $ getEntriesMarked db vars name


-- |Analyzes a rule, finding all dynamic variables.
extractVars :: Rule -> [String]
extractVars = extractRule $ filter (isUpper . head) . snd


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


createVarMap ::Vector2 (String, String) -> Variables
createVarMap = V.foldl' go M.empty
	where
		go acc = M.unionWith vunion acc . V.foldl' go' M.empty
		go' acc (name,val) = M.insertWith vunion name (return val) acc


-- |Extends the root wrapper giving back the information with variables marked.
getEntriesMarked :: Database -> [String] -> String -> Vector2 (String, String)
getEntriesMarked db@(Database m) vars = evalVariables vars . getEntries db


-- |Root wrapper for getting database information.
getEntries :: Database -> String -> Vector2 String
getEntries db@(Database m) query = case M.lookup query $ dmap db of
	Just x -> x
	Nothing -> V.empty


-- This function provides the desired variable combinations.
evalVariables :: [String] -> Vector2 String -> Vector2 (String, String)
evalVariables vars rs
		| all (isLower . head) vars = staticVarEval vars
		| otherwise  = V.filter ((== V.length (V.head rs)). V.length) $ V.map (go $ V.fromList vars) rs
	where
		go vs rs = if V.null vs 
						then V.empty 
						else if V.null rs
							then let (v1, v2) = V.splitAt 1 vs 
								in ("_", V.head v1) `V.cons` go V.empty v2
							else let 
									(v,v2) = vpatt vs
									(r,r2) = vpatt rs
								in if isUpper (head v) 
							 		then (v, r) `V.cons` go v2 r2
							 		else if v == r then ("_", r) `V.cons` go v2 r2
							 		else V.empty


-- Extra vector functions that did not exist in the standard vector library.
vnub :: Eq a => Vector a -> Vector a
vnub xs = go (xs, V.empty) V.empty
	where
		go (vss,acc) existing 
			| V.null vss = acc
		 	| otherwise = let (v,vs) = vpatt vss in
					if V.elem v existing 
						then go (vs, acc) existing
						else go (vs, V.cons v acc) $ V.cons v existing


vpatt :: Vector a -> (a, Vector a)
vpatt = (\(a,b) -> (V.head a, b)) . V.splitAt 1


staticVarEval :: [String] -> Vector2 (String, String)
staticVarEval = return . V.map ("_",) . V.fromList


vcatMaybes :: Vector (Maybe a) -> Vector a
vcatMaybes = V.map fromJust . V.filter isJust


vunion :: Eq a => Vector a -> Vector a -> Vector a
vunion v1 v2 = (v2 V.++) $ V.filter (`V.notElem`v2) v1


vlookup :: Ord a => a -> Vector (a, b) -> Maybe b
vlookup query xs = if V.null xs 
						then Nothing 
						else 
							let (x1, x2) = V.splitAt 1 xs
							    (n,v) = V.head x1
							in if n == query
								then Just v
								else vlookup query x2



-- Testing
tests = test [
		"dummy" ~: True ~=? 1 == 1
	]

-- For debugging
debug :: IO Database
debug = readFile "test.txt" >>= (return . fromJust . compile')