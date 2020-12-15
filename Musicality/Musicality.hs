--  File    : Musicality.hs
--  Author  : Andrew Naughton <naughtona@student.unimelb.edu.au>
--  Origin  : Mon 26 Oct 11:20:45 2020
--  Purpose : Project 2 Submission, Declarative Programming COMP30020, S2 2020

--  This file implements functions that define an agent that plays 'Game of
--  Musician' as a Composer as well as one that plays as a Performer.

--  'Game of Musician' is a logical guessing game where a 'Composer' picks a 
--  'Chord'. A Chord is a list of three distinct 'Pitch' elements. The other 
--  part of this game is played by a 'Performer', whose job it is to guess the
--  Chord, via repeated guesses, informed by feedback in the form of an 
--  'Answer'. The 'feedback' function provides this feedback.

--  The Performer Agent solves this problem via generating  'allChords' and
--  removing previous incorrect guesses. We further prune this list by only 
--  selecting 'viableGuesses', which reduces the solution space substantially. 
--  Documentation accompanying 'nextGuess', 'selectBestGuess', and 
--  'expNGuesses' elaborate on our method for deciding on a smart next guess.

--  Average number of guesses over all 1330 possible targets is 4.209.

module Musicality (Pitch(..), toPitch, feedback,
                   GameState, initialGuess, nextGuess) where

import Data.List
import Data.Ord (comparing)

-----------------------       DATA PREP       --------------------------------

-- | Pitch is a Pitch with two characters, one for its note ('A'..'G') and the
--   other for its octave ('1'..'3'). It derives the Eq class, so that pitches
--   can be compared/equated.
data Pitch = Pitch Char Char deriving (Eq)


-- | Instance declaration to make Pitch type an instance of the Show class, so
--   that pitches are nicely displayed. Makes use of pitchToString.
instance Show Pitch where
    show (Pitch note oct) = [note, oct]


-- | Answer is the feedback provided for a guess, represented by a 3-integer-
--   element tuple; 
--   Chord is a list of 'Pitch' elements; and 
--   GameState holds a list of remaining possible Chords.
type Answer = (Int,Int,Int)
type Chord = [Pitch]
type GameState = [Chord]


-----------------------       COMPOSER        --------------------------------

-- | Takes a String representing the concatenation of a note (char) and octave
--   (char), given by pitch. Returns Just the Pitch named by the pitch, or 
--   Nothing if the pitch is not a valid pitch name.
toPitch :: String -> Maybe Pitch
toPitch [note, oct] = if note `elem` ['A'..'G'] && oct `elem` ['1'..'3']
                      then Just (Pitch note oct)
                      else Nothing
toPitch _  = Nothing


-- | Takes a target Chord and a guess Chord, respectively, and returns the
--   appropriate feedback given by an Answer, as defined by the project spec.
--   Will error if the target contains any other than 3 different Pitch 
--   elements.
--   The Answer comprises:
--   'correct': how many pitches in the guess are included in the target.
--   'correctNote': how many pitches have the right note but the wrong octave.
--   'correctOct': how many pitches have the right octave but the wrong note.
feedback :: Chord -> Chord -> Answer
feedback target guess
    | validChord target = (correct, correctNote, correctOct)
    | otherwise         = error "Error: Invalid Target Chord Length"
    where target'     = [[note,oct] | (Pitch note oct) <- target]
          guess'      = nub [[note,oct] | (Pitch note oct) <- guess]
          correct     = length $ target' `intersect` guess'
          targNotes   = map (!!0) target'
          targOcts    = map (!!1) target'
          guesNotes   = map (!!0) guess'
          guesOcts    = map (!!1) guess'
          notePairs   = nPairs guesNotes targNotes 0
          correctNote = notePairs - correct
          octPairs    = nPairs guesOcts targOcts 0
          correctOct  = octPairs - correct


-----------------------    COMPOSER HELPERS    -------------------------------

-- | Takes a Chord and returns whether or not the passed chord is valid. A
--   valid Chord is a list of exactly 3 elements with no duplicates.
validChord :: Chord -> Bool
validChord chord = length chord == 3 && nub chord == chord


-- | Polymorphic type function that takes a guess and target (both lists) and
--   an accumulator integer, and returns the number of pairs that can be found 
--   in guess and target, where paired elements are removed from target. The
--   lists can be of any data type that can be equated ie. part of Eq class.
nPairs :: Eq a => [a] -> [a] -> Int -> Int
nPairs [] target pairs = pairs
nPairs (g:gs) target pairs
    | g `elem` target = nPairs gs (g `delete` target) (pairs+1)
    | otherwise       = nPairs gs target pairs


-----------------------       PERFORMER        -------------------------------

-- | Takes no input arguments, and returns a pair of an initial guess Chord
--   and an initial GameState. The initial guess is the guess that derives the
--   lowest expected number of guesses for all 1330 possible targets. The 
--   initial GameState is a list of all chords yet to be guessed.
initialGuess :: (Chord, GameState)
initialGuess = (initguess, state)
    where initguess = [Pitch 'A' '2', Pitch 'B' '1', Pitch 'C' '1']
          state     = initguess `delete` allChords


-- | Takes a pair of the previous guess Chord and GameState, and the feedback
--   to this guess as an Answer, and returns a pair of our next guess Chord
--   and the next GameState. The next GameState only includes those chords
--   that would derive the same feedback for the previous guess, if they were
--   the guess and the previous guess was the target. After pruning the
--   GameState like this, the next guess is generated.
nextGuess :: (Chord, GameState) -> Answer -> (Chord, GameState)
nextGuess (prevGuess, prevState) prevAnswer = (nextGuess, nextState')
    where nextState  = prevGuess `delete` prevState
          nextState' = viableGuesses nextState prevGuess prevAnswer
          nextGuess  = selectBestGuess nextState'


-- | Takes a GameState, target Chord, and Answer, and calculates all feedback
--   results between the target and each Chord in GameState. We prune any 
--   Chords from GameState that produce feedback different to the passed-in 
--   Answer, thus, only retaining viable guesses.
viableGuesses :: GameState -> Chord -> Answer -> GameState
viableGuesses state target answer = state'
    where state' = [chord | chord <- state, feedback target chord == answer]


-- | Takes the current GameState, and returns the best available guess Chord. 
--   Best is defined as the guess that derives the minimum expected remaining 
--   guesses, assuming all remaining guesses are not the target.
selectBestGuess :: GameState -> Chord
selectBestGuess state = fst $ minimumBy (comparing snd) allNGuesses
    where allNGuesses = [(target, nGuess) | target <- state
                        , let state' = target `delete` state
                        , let nGuess = expNGuesses target state']


-- | Takes some possible target Chord and the current GameState, and returns
--   the expected number of remaining guesses, given the passed-in target is
--   not the actual target in the game.
--
--   Method:
--   1. Generate and sort a list of possible answers for the passed-in target.
--   2. Count the total number of answers calculated.
--   3. Group these answers, and count how many guesses are in each group 
--   (groupsizes).
--   Note that the size of each group represents a likelihood as the number
--   of guesses to remain, given the possible target is chosen as next guess.
--   4. With this information, we calculate the expected number of remaining
--   guesses. The lower the number, the better the guess is at pruning the 
--   game state in averaged terms.
--
--   See expValue for an explanation of how we calculate the average.
expNGuesses :: Chord -> GameState -> Double
expNGuesses target state = avesize
    where answers    = sort $ map (\guess -> feedback target guess) state
          nAnswers   = length answers
          groupsizes = map length $ group answers
          avesize    = sum $ map (\size -> expValue size nAnswers) groupsizes


-----------------------    PERFORMER HELPERS    ------------------------------

-- | Calculates expected value given two integers. The first Int represents
--   how many times the event happened (occurences), the second Int represents
--   how many times all events happened (allOccurences).
--   Expected/Average Value = likelihood * occurences
--   Where likelihood is the occurences / allOccurences.
expValue :: Int -> Int -> Double
expValue occurences allOccurences = occurences' * likelihood
    where occurences'    = fromIntegral occurences
          allOccurences' = fromIntegral allOccurences
          likelihood     = occurences' / allOccurences'


-- | Generate a list of all 1330 possible Chords, where a Chord is list of
--   three distinct 'Pitch' elements.
allChords :: [Chord]
allChords = [chord | chord <- pitchSubseqs, length chord == 3]
    where pitchSubseqs = subsequences allPitches


-- | Generate a list of all 21 possible pitches, where a pitch consists of a
--   note ('A'..'G') and an octave ('1'..'3')
allPitches :: [Pitch]
allPitches = [Pitch note oct | note <- ['A'..'G'], oct <- ['1'..'3']]