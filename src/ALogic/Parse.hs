module ALogic.Parse (
		parseAL
		,tests
	) where

import Text.ParserCombinators.Parsec hiding(spaces)
import Text.Parsec hiding (try, spaces)
import Data.Maybe (isJust, catMaybes, fromJust)
import Control.Monad (liftM, void)
import Control.Applicative ((<*>))
import Test.HUnit

import ALogic.Core(Rule(..))


-- |This data type is used for the parser and the parser modules 
-- task is to take a ALString and convert it into a set of rules
-- with the ALVal data type as a hidden middle layer.
data ALVal = Var String [String] | 
			Val String | 
			OP String | 
			NoVal String | 
			Compiled Rule |
			Para [ALVal]
	deriving(Show, Eq)


-- |All the default symbols.
defaultSymbols :: String
defaultSymbols = "-<>!&|()"


-- |A lookup list of all the operators.
ops :: [(String, Rule -> Rule -> Rule)]
ops = [
		("<-", Imply),
		("!<-", ImplyNot),
		("&", And),
		("|", Or),
		("&!", AndNot)
	]


-- |This function creates a Relation if the ALVal is a Var else Nothing.
makeRel :: ALVal -> Maybe Rule
makeRel (Var n xs) = Just $ Relation n xs
makeRel _ = Nothing


-- |Takes a lookup list with operators and their actions, and then tries to create a rule from it.
compileOPs :: [(String, Rule -> Rule -> Rule)] -> String -> ALVal -> [ALVal] -> Maybe Rule
compileOPs ops s a bs = lookup s ops <*> constructData [a] <*> constructData bs


-- |Given a set of ALVals tries to find a way to infer a rule, else return Nothing.
constructData :: [ALVal] -> Maybe Rule
constructData (a:OP s:bs) = compileOPs ops s a bs
constructData [x@(Var n xs)] = makeRel x
constructData [x@(Para xs)] = constructData xs
constructData _ = Nothing


-- |The parseAL function takes a AL string and turns it into
-- a Maybe list of Maybe rules.
parseAL :: String -> Maybe [Maybe Rule]
parseAL s = liftM (map constructData) $ case parse rules "Parse error" s of
	Left _ -> Nothing
	Right x -> Just x


-- |Parses a set of Rules in ALVal format.
rules :: Parser [[ALVal]]
rules = sepBy1 rule $ do char ';'; skipMany $ try $ char '\n' <|> char ' '


-- |Parse a Rule in ALVal format.
rule :: Parser [ALVal]
rule = sepBy1 var spaces


-- |This function tries all combinations of ALVals until it finds one.
var :: Parser ALVal
var = choice [
			try (para (liftM Para rule)),
			try pVar,
			try (liftM OP $ many1 $ oneOf defaultSymbols),
			try (liftM Val $ many1 $ letter <|> digit), 
			try (liftM NoVal $ many anyChar)
		]


-- |Parses a Var.
pVar :: Parser ALVal
pVar = do
	char '$'
	r <- endBy (many1 $ letter <|> digit) $ try symbols <|> spaces
	return $ Var (head r) $ tail r


-- |Just skipps some spaces.
spaces :: Parser ()
spaces = skipMany1 space


-- |Used to find out if the next character is a symbol without reading it.
symbols :: Parser ()
symbols = lookAhead $ void $ many space >> oneOf (defaultSymbols ++ ";")


-- |Used to parse everything between two paranthesis.
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