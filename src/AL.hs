module AL (
		compile,
		query,
		Database(..)
	) where

import AL.Compile
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad

compile :: String -> Maybe Database
compile = compile'

-- |This function should be used to state
-- queries to an AL database.
query :: Database -> String -> Maybe [[String]]
query c s = liftM (map V.toList) $ liftM V.toList $ M.lookup s $ dmap c