import qualified Data.Map as M

data Database = Database {
						dmap :: (M.Map String [String])
					}

data Rule = Relation String String | Imply String String
	deriving(Show)
type Clause = [Rule]

main :: IO ()
main = do
	let c = emptyClause
	print $ query [
			(Relation "band" "bob"),
			(Relation "band" "tom"),
			(Relation "band" "pete"),
			(Imply "sit" "drummer"),
			(Relation "sit" "bob")
		] "drummer"

query :: Clause -> String -> [String]
query c s = ((M.! s) . dmap . compileClause) c

emptyClause :: Clause
emptyClause = []

compileClause :: Clause -> Database
compileClause = foldr evalRule db
	where
		db = Database M.empty

evalRule :: Rule -> Database -> Database
evalRule r (Database m) = Database (insertion m)
				where 
					insertion = case r of
						(Relation tar val) -> M.insertWith (++) tar [val]
						(Imply tar val) -> M.insertWith (++) val (m M.! tar)