-- See Fermat's Little Theorem
-- Given a candidate n, it picks a random number a < n and finds: a^n (mod n).
-- If this differs from a, then n is not a prime, by Fermat's theorem.
-- Otherwise it may be. Now repeat with another random number a, up to 100
-- independent tests. If they all agree that n is prime, then it reports that
-- this is the case.

import System.Random

--  Fast probabilistic primality checking:

--  Generate n random integers in the range lower .. upper:
randomList :: Int -> Integer -> Integer -> IO [Integer]
randomList n lower upper
  = do
      gen <- getStdGen
      let rs = randomRs (lower, upper) gen
      return (take n rs)

--  Calculate b^e mod m:
expmod :: Integer -> Integer -> Integer -> Integer
expmod b e m
  | e == 0    = 1
  | even e    = square (expmod b (e `div` 2) m) `mod` m
  | otherwise = (b * (expmod b (e-1) m)) `mod` m
    where
      square n = n*n

--  The number of times we want to repeat Fermat's test for a given candidate:
fermatRepeats :: Int
fermatRepeats = 100

--  The test itself. The type is an input/output action, as randomRs is monadic.
fermat :: Integer -> IO ()
fermat n
  = do
      randoms <- randomList fermatRepeats 2 (n-1)
      let looksLikePrime = and [a == expmod a n n | a <- randoms]
      let qual = if looksLikePrime then "" else "not "
      putStr (show n ++ " is " ++ qual ++ "prime\n")