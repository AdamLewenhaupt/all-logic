module AL (
		compile,
		query
	) where

import qualified AL.Compile as C
import qualified Data.Map as M

compile :: String -> Maybe C.Database
compile = C.compile

-- |This function should be used to state
-- queries to an AL database.
query :: C.Database -> String -> [[String]]
query c s = C.dmap c M.! s