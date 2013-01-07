module ALogic (
		compile,
		query,
		Database(..)
	) where

import ALogic.Compile
import ALogic.Core

import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad

compile :: String -> Maybe Database
compile = compile'

createDB :: [Rule] -> Database
createDB = createDB'

-- |This function should be used to state
-- queries to an AL database.
query :: Database -> String -> Maybe [[String]]
query c s = liftM (map V.toList . V.toList) $ M.lookup s $ dmap c