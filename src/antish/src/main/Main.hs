module Main where

import Parser
import System.Directory 


-- TODO
main :: IO ()
main = undefined
{--do
	args <- getArgs
	let [file] = args
	f <- readFile testfile -- or file or file <- getContents ?
	print $ parse pProgram "" f
 --}


testfile :: FilePath
testfile = "test.ha" -- is likely to fail as there is no testing file in the folder

{- 

This  code worked in my file but because the stars are not correctly alligned of some dark magic I get this error ...


src/main/Main.hs:23:12:
    Unexpected semi-colons in conditional:
        if exiting then do { f <- readFile file;
                             print $ parse pProgram "" f }; else print "Stupid you"
    Perhaps you meant to use -XDoAndIfThenElse?


testing file = do
	exiting <- doesFileExist file
	if exiting then do
		f <- readFile file
		print $ parse pProgram "" f
	else 
		print "Stupid you"
-}

