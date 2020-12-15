--  Author   : Peter Schachte
--  Purpose  : Test program
--  Copyright: (c) 2020 The University of Melbourne

module Main where

import Data.List
import Data.Maybe
import Musicality

-- | Guess the given target, counting and showing the guesses.
guessTest :: [Pitch] -> IO ()
guessTest target = do
			let (guess,other) = initialGuess
			loop target guess other 1

-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Pitch] -> [Pitch] -> GameState -> Int -> IO ()
loop target guess other guesses = do
	putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ show guess
	let answer = feedback target guess
	putStrLn $ "    My answer:  " ++ show answer
	if answer == (3,0,0)
		then do
			putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
		else do
			let (guess',other') = nextGuess (guess,other) answer
			loop target guess' other' (guesses+1)

-- | Parse a string containing a number of space-separated pitches to produce
-- a list of pitches.  Error if any of the pitches can't be parsed.
toChord :: String -> [Pitch]
toChord = (fromJust . mapM toPitch . words)

-- | Prompt for a target and use guessTest to try to guess it.
main :: IO ()
main = do
putStr "Target chord (3 pitches separated by spaces): "
	text <- getLine
	guessTest $ toChord text

-- Prelude> :l PeterTest
-- [1 of 2] Compiling Musicality       ( Musicality.hs, interpreted )
-- [2 of 2] Compiling Main             ( PeterTest.hs, interpreted )
-- Ok, modules loaded: Main, Musicality.
-- *Main> main
-- Target chord (3 pitches separated by spaces): A2 F2 G2
-- Your guess #1:  [A2,B1,C1]
-- 	My answer:  (1,0,0)
-- Your guess #2:  [A2,D3,E2]
-- 	My answer:  (1,0,1)
-- Your guess #3:  [A2,A3,F3]
-- 	My answer:  (1,1,0)
-- Your guess #4:  [A2,F2,G2]
-- 	My answer:  (3,0,0)
-- You got it in 4 guesses!