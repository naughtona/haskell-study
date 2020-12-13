merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y     = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys
    
msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort (take mid xs)) (msort (drop mid xs))
    where mid  = (div (length xs) 2)


-- ~/uni/comp30020/gh/haskell/practice >>> ghci MergeSort
-- GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
-- [1 of 1] Compiling Main             ( MergeSort.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main> msort [3,1,5,9,8,4,7,2,6,3]
-- [1,2,3,3,4,5,6,7,8,9]