--  To read a number from standard input:

getInt :: IO Int
getInt
  = do
      line <- getLine
      return ((read line) :: Int)

--  Implement the main function. It should ask for a heap size, then enter an 
--  input-output loop in which it plays its move and accepts the user's move.

main :: IO ()
main
  = do
      putStr "heap size:                 "
      n <- getInt
      play n

--  Implement the function that calculates how many beans to remove, 
--  given a current heap of size `heap_size`.

move :: Int -> Int
move heap_size
  | heap_mod_3 == 0 = 1       -- we're losing, take 1 and hope for the best
  | otherwise = heap_mod_3    -- we're winning
	where heap_mod_3 = heap_size `mod` 3

--  `play' takes the current heap size as argument. This is the main loop:

play :: Int -> IO ()
play heap_size
  = do
    	let our_move = move heap_size
      	putStr ("I take:             " ++ show our_move ++ " \n")
      	if (our_move == heap_size)
        then do
            putStrLn "I won!"
        else do
            putStrLn ("This leaves:               " 
                        ++ show (heap_size - our_move))
            their_move <- response
            let new_heap_size = heap_size - (our_move + their_move)
            if (new_heap_size == 0)
            then do
                putStrLn "You won!"
            else do
                putStrLn ("This leaves:               " 
                            ++ show new_heap_size)
                play new_heap_size

response :: IO Int
response
  = do
      putStr "You take how many?  "
      their_move <- getInt
      if their_move < 1 || their_move > 2
        then do
              putStrLn ("Sorry, it has to be 1 or 2")
              response
        else return their_move