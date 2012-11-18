module AL.Core (
		Rule(..),
		Clause
	) where

-- |The Rule type provides all rules that the AL compiler supports.
data Rule = Relation String [String] | Imply String [String] String [String]
	deriving(Show)

-- |The clause data-type provides a wrapper for a list of rules as
-- a wrapper for later improvements.
type Clause = [Rule]