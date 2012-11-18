module AL.Parse (
		parseAL
	) where

import Text.ParserCombinators.Parsec hiding(spaces)
import Text.Parsec hiding (try, spaces)
import Data.Maybe (isJust, catMaybes, fromJust)
import Control.Monad (liftM)

import AL.Core(Rule(..))

data ALVal = Var String [String] | Val String | OP String | NoVal String

compileRule :: [ALVal] -> Maybe Rule
compileRule [(Var n1 xs1),(OP "->"),(Var n2 xs2)] = Just $ Imply n1 xs1 n2 xs2
compileRule [(Var n xs)] = Just $ Relation n xs
compileRule _ = Nothing

-- |The parseAL function takes a AL string and turns it into
-- a Maybe list of Maybe rules.
parseAL :: String -> Maybe [Maybe Rule]
parseAL s = liftM (map compileRule) $ case parse rules "Parse error" s of
	Left _ -> Nothing
	Right x -> Just x

rules :: Parser [[ALVal]]
rules = sepBy1 rule $ do char ';'; skipMany $ char '\n'

rule :: Parser [ALVal]
rule = sepBy1 var spaces

var :: Parser ALVal
var = choice [
			(liftM OP $ many1 $ oneOf "->"),
			pVar,
			(liftM Val $ many1 $ letter <|> digit), 
			(liftM NoVal $ many anyChar)
		]


pVar :: Parser ALVal
pVar = do
	char '$'
	r <- endBy (many1 $ letter <|> digit) $ (try symbols) <|> spaces
	return $ Var (head r) $ tail r

spaces :: Parser ()
spaces = skipMany1 $ space

symbols :: Parser ()
symbols = lookAhead $ many space >> oneOf "->;" >> return ()