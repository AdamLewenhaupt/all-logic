module Main (main) where

import qualified AL.Parse as P
import qualified AL.Compile as C
import Test.HUnit

main :: IO ()
main = do
	fails <- fmap (sum . map failures) $ sequence $ map runTestTT [P.tests, C.tests]
	putStrLn $ if fails > 0 then "Failures detected" else "All unit tests passed"