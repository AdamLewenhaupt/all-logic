module AL.Parse (
		parseAL
		,tests
	) where

import Text.ParserCombinators.Parsec hiding(spaces)
import Text.Parsec hiding (try, spaces)
import Data.Maybe (isJust, catMaybes, fromJust)
import Control.Monad (liftM)
import Control.Applicative ((<*>))
import Test.HUnit

import AL.Core(Rule(..))

data ALVal = Var String [String] | 
			Val String | 
			OP String | 
			NoVal String | 
			Compiled Rule |
			Para [ALVal]
	deriving(Show, Eq)

defaultSymbols :: String
defaultSymbols = "-<>!&|()"

ops :: [(String, (Rule -> Rule -> Rule))]
ops = [
		("<-", Imply),
		("!<-", ImplyNot),
		("&", And),
		("|", Or),
		("&!", AndNot)
	]


makeRel :: ALVal -> Maybe Rule
makeRel (Var n xs) = Just $ Relation n xs
makeRel _ = Nothing


compileOPs :: [(String, (Rule -> Rule -> Rule))] -> String -> ALVal -> [ALVal] -> Maybe Rule
compileOPs ops s a bs = lookup s ops <*> constructData [a] <*> constructData bs


constructData :: [ALVal] -> Maybe Rule
constructData (a:(OP s):bs) = compileOPs ops s a bs
constructData [x@(Var n xs)] = makeRel x
constructData [x@(Para xs)] = constructData xs
constructData _ = Nothing

-- |The parseAL function takes a AL string and turns it into
-- a Maybe list of Maybe rules.
parseAL :: String -> Maybe [Maybe Rule]
parseAL s = liftM (map constructData) $ case parse rules "Parse error" s of
	Left _ -> Nothing
	Right x -> Just x

rules :: Parser [[ALVal]]
rules = sepBy1 rule $ do char ';'; skipMany $ try $ char '\n' <|> char ' '

rule :: Parser [ALVal]
rule = sepBy1 var spaces

var :: Parser ALVal
var = choice [
			try (para (liftM Para $ rule)),
			try pVar,
			try (liftM OP $ many1 $ oneOf defaultSymbols),
			try (liftM Val $ many1 $ letter <|> digit), 
			try (liftM NoVal $ many anyChar)
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


para :: Parser a -> Parser a
para = between (char '(') (char ')')


-- Testing
tests = test [
		"para parse" ~: Just (Var "name" ["adam"]) ~=? case parse (para var) "err" "($name adam)" of Right x -> Just x; Left _ -> Nothing
		,"simple eval" ~: Just [Just $ Relation "love" ["romeo", "julia"], Nothing] ~=? parseAL "$love romeo julia;"
		, "and not" ~: Just [Just $ AndNot (Relation "name" ["X"]) (Relation "eat" ["X", "bacon"]), Nothing] ~=? parseAL "$name X &! $eat X bacon;"
		, "para" ~: Just [Just $ And (And (Relation "human" ["X"]) (Relation "live" ["X"]) ) (Relation "eat" ["X"]), Nothing] ~=? parseAL "($human X & $live X) & $eat X;"
		, "complex para" ~: Just [Just $ AndNot (And (Relation "love" ["Y", "Z"]) (Relation "love" ["Z", "Y"]) ) (And (Relation "love" ["X", "Z"]) (Relation "love" ["Z", "X"]) ), Nothing] ~=? parseAL "($love Y Z & $love Z Y) &! ($love X Z & $love Z X);"
	]