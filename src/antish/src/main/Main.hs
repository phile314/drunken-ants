module Main where

import Parser
import System.Environment

-- TODO
main :: IO ()
main = do
  args <- getArgs
  let [file] = args
  ast <- parseFile file
  print ast
  
 
{-
testing file = do
	exiting <- doesFileExist file
	if exiting then do
		f <- readFile file
		print $ parse pProgram "" f
	else 
		print "Stupid you"
-}

