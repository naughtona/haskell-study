--  File    : MyTest.hs
--  Author  : Andrew Naughton <naughtona@student.unimelb.edu.au>
--  Purpose : Test Program

module Main where

import Data.List
import Musicality

type Chord = [Musicality.Pitch]


averageGuessRate :: Double
averageGuessRate = averageN
    where averageN = average [guessTest target | target <- allChords]

average :: [Int] -> Double
average lst = lstsum / lstlen
    where lstsum = fromIntegral $ sum lst
          lstlen = fromIntegral $ length lst

-- | Guess the given target, counting and showing the guesses.
guessTest :: Chord -> Int
guessTest target = nGuess
    where (guess, state) = initialGuess
          nGuess         = loop target guess state 1

-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: Chord -> Chord -> Musicality.GameState -> Int -> Int
loop target guess other guesses = 
    let answer = feedback target guess in
    if answer == (3,0,0)
        then guesses
    else 
        let (guess',other') = nextGuess (guess,other) answer in
        loop target guess' other' (guesses+1)

-- generate all 1330 possible chords
allChords :: [Chord]
allChords = [chord | chord <- pitchSubseqs, length chord == 3]
    where pitchSubseqs = subsequences allPitches

-- generate all 21 possible pitches
allPitches :: [Pitch]
allPitches = [Pitch note oct | note <- ['A'..'G'], oct <- ['1'..'3']]

-- | Prompt for a target and use guessTest to try to guess it.
main :: IO ()
main = print averageGuessRate


-- Prelude> :l MyTest
-- [1 of 2] Compiling Musicality       ( Musicality.hs, interpreted )
-- [2 of 2] Compiling Main             ( MyTest.hs, interpreted )
-- Ok, modules loaded: Main, Musicality.
-- *Main> main
-- 4.209022556390978