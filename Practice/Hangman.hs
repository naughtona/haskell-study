module Main where

import Data.Char
import System.Random

--  'main' selects a word to be guessed, then enters a dialogue with the
--  user to help them determine the secret word:
main :: IO ()
main
  = do
      dict <- readFile "words.txt"   -- get contents of file "words.txt"
      let words = lines dict         -- separate it into lines (here: words)
      let len = length words         -- count how many words we have
      play words len                 -- now play the game

--  The function 'play' allows the user to play again and again:
play :: [String] -> Int -> IO ()
play words n
  = do
      putStr "Want a challenge (y/n)? "
      answer <- getLine
      if (head answer) == 'y'
      then 
        do
          putStrLn "Okay, here it is:"
          ran <- getStdRandom (randomR (0,n-1))  -- random number in range
          let w = words!!ran                     -- w is the secret word;
          solve w []                             -- [] initial list of guesses
          play words n
      else 
        do
          putStrLn "Okay, bye"
          return ()

--  The interactive solver loop:
solve :: String -> String -> IO ()
solve word guesses
  = do
      let indic = indicator word guesses
      putStr ("         " ++ indic)
      if (all isAlpha indic)
        then putStrLn ("   " ++ show (length guesses) ++ " guesses")
        else do
               putStrLn ""
               guess <- response
               solve word (guess:guesses)

indicator :: String -> String -> String
indicator w gs
  = [if c `elem` gs then c else '-' | c <- w]

--  Handling the user's input may also require a loop (if input is invalid):
response :: IO Char
response
  = do
      putStr "guess> "
      gs <- getLine
      if length gs == 1 && isAlpha (head gs)
        then return (toLower (head gs))
        else do
              putStrLn "Just a single letter!"
              response