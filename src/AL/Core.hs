module AL.Core (
		Rule(..)
	) where

-- |The Rule type provides all rules that the AL compiler supports.
data Rule = Relation String [String] | 
			Imply Rule Rule |
			ImplyNot Rule Rule |
			And Rule Rule |
			Or Rule Rule |
			AndNot Rule Rule
	deriving(Show, Eq)