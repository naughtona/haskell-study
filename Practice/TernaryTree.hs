import Data.Ord
import Data.List

data Ttree t = Nil | Node3 t (Ttree t) (Ttree t) (Ttree t)

-- | Returns True if two Ternary trees have the same shape, else False
same_shape :: Ttree t -> Ttree t -> Bool
same_shape Nil Nil = True
same_shape Nil _   = False
same_shape _   Nil = False
same_shape (Node3 _ l m r) (Node3 _ l' m' r') = left && mid && right
    where left  = same_shape l l'
          mid   = same_shape m m'
          right = same_shape r r'


-- | Given a ttree of lists, shortest_concat returns the shortest list
--   in the tree and the concatenation of all the lists in the tree in
--   the following order:
--      - the list from the root
--      - the lists from the left subtree
--      - the lists from the middle subtree
--      - the lists from the right subtree
--   Note this performs a single traversal over the tree and has O(N) worst
--   case time complexity
shortest_concat :: Ttree [t] -> ([t],[t])
shortest_concat = shortest_concat' []

shortest_concat' :: [t] -> Ttree [t] -> ([t],[t])
shortest_concat' curr  Nil              = (curr, [])
shortest_concat' _    (Node3 lst l m r) = (shortest, conc)
    where (lshortest, lconc) = shortest_concat' lst l 
          (mshortest, mconc) = shortest_concat' lst m 
          (rshortest, rconc) = shortest_concat' lst r
          shortest = minimumBy (comparing length) [lshortest, mshortest, rshortest]
          conc = lst ++ lconc ++ mconc ++ rconc


-- ~/uni/comp30020/gh/haskell/practice >>> ghci TernaryTree
-- GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
-- [1 of 1] Compiling Main             ( TernaryTree.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main> same_shape (Node3 [1,2,3] Nil Nil Nil) (Node3 [1,2] Nil Nil Nil)
-- True
-- *Main> same_shape (Node3 [1,2,3] Nil Nil Nil) (Node3 [1,2] Nil Nil (Node3 [1,2,3] Nil Nil Nil))
-- False
-- *Main> shortest_concat (Node3 [1,2,3] (Node3 [1,2] Nil Nil Nil) (Node3 [1,2,3,4,5] Nil (Node3 [1] Nil Nil Nil) Nil) (Node3 [1,2,3,4,5,6,7,8] Nil Nil Nil))
-- ([1],[1,2,3,1,2,1,2,3,4,5,1,1,2,3,4,5,6,7,8])
