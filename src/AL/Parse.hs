module AL.Parse (
		parseAL
	) where

import Text.ParserCombinators.Parsec hiding(spaces)
import Text.Parsec hiding (try, spaces)
import Data.Maybe (isJust, catMaybes, fromJust)
import Control.Monad (liftM)
import Control.Applicative ((<*>))

import AL.Core(Rule(..))

data ALVal = Var String [String] | Val String | OP String | NoVal String | Compiled Rule
	deriving(Show)

defaultSymbols :: String
defaultSymbols = "-<>!&|"

ops :: [(String, (Rule -> Rule -> Rule))]
ops = [
		("<-", Imply),
		("!<-", ImplyNot),
		("&", And),
		("|", Or)
	]


makeRel :: ALVal -> Maybe Rule
makeRel (Var n xs) = Just $ Relation n xs
makeRel _ = Nothing


compileOPs :: [(String, (Rule -> Rule -> Rule))] -> String -> ALVal -> [ALVal] -> Maybe Rule
compileOPs ops s a bs = lookup s ops <*> constructData [a] <*> constructData bs


constructData :: [ALVal] -> Maybe Rule
constructData (a:(OP s):bs) = compileOPs ops s a bs
constructData [x@(Var n xs)] = makeRel x
constructData _ = Nothing

-- |The parseAL function takes a AL string and turns it into
-- a Maybe list of Maybe rules.
parseAL :: String -> Maybe [Maybe Rule]
parseAL s = liftM (map constructData) $ case parse rules "Parse error" s of
	Left _ -> Nothing
	Right x -> Just x

rules :: Parser [[ALVal]]
rules = sepBy1 rule $ do char ';'; skipMany $ char '\n'

rule :: Parser [ALVal]
rule = sepBy1 var spaces

var :: Parser ALVal
var = choice [
			(liftM OP $ many1 $ oneOf defaultSymbols),
			pVar,
			(liftM Val $ many1 $ letter <|> digit), 
			(liftM NoVal $ many anyChar)
		]

pVar :: Parser ALVal
pVar = do
	char '$'
	r <- endBy (many1 $ letter <|> digit) $ try symbols <|> spaces
	return $ Var (head r) $ tail r

spaces :: Parser ()
spaces = skipMany1 $ space

symbols :: Parser ()
symbols = lookAhead $ many space >> oneOf (defaultSymbols ++ ";") >> return ()