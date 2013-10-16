module Main where

import Parser
import System.Environment
import Ast

-- TODO
main :: IO ()
main = do
  args <- getArgs
  let [file] = args
  res <- parseFile file
  case res of
    (Right t) -> print (UseTree t)
    (Left er) -> print er
  
 
{-
testing file = do
	exiting <- doesFileExist file
	if exiting then do
		f <- readFile file
		print $ parse pProgram "" f
	else 
		print "Stupid you"
-}

