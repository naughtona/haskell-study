qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (pivot:xs) = qsort lesser ++ [pivot] ++ qsort greater
   where
      lesser  = filter (< pivot)  xs
      greater = filter (>= pivot) xs


-- ~/uni/comp30020/gh/haskell/practice >>> ghci QuickSort
-- GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
-- [1 of 1] Compiling Main             ( QuickSort.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main> qsort [3,1,5,9,8,4,7,2,6,3]
-- [1,2,3,3,4,5,6,7,8,9]