# AllLogic 

## Introduction

All-logic provides a way for haskellers to use basic logic
statements to build logical rules that can be used easily
in haskell.

In order to simplify the library one can choose to use it as an interpreter that parses text files and gives back a Database that can be queried.

If speed is of the essence it's also possible to create a database directly from the datatypes in the ALogic.Core module.

## Syntax

The parser uses a quite simple syntax, for example:

	$love romeo julia;
	$love julia romeo;
	$love peter julia;

	$happy X <- $love X Y & $love Y X;
	$unhappy X <- $love X Y &! $love Y X;
	$jealous X Z <- ($love X Y & $love Z Y) & $love Y Z; 

Is pretty much equal to:
	
	romeo love julia
	julia love romeo
	peter love julia

	all X are happy where X love Y and Y love X
	all X are unhappy where X love Y and not Y love X
	all X are jealous of Z where X love Y and Z love Y and Y love Z

## Example

As an example the following program given that the file test.txt contains the above code gives back: Just [["peter","romeo"]]

	import ALogic

	main = do
		text <- readFile "test.txt"
		case compile text of
			Nothing -> putStrLn "Invalid query :("
			Just db -> print $ query db "jealous"
