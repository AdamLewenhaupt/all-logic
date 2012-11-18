module AL (
		compile,
		query,
		Database(..)
	) where

import AL.Compile
import qualified Data.Map as M

compile :: String -> Maybe Database
compile = compile'

-- |This function should be used to state
-- queries to an AL database.
query :: Database -> String -> [[String]]
query c s = dmap c M.! s