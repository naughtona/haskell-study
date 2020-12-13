-- Multi-way trees are trees in which a node may have an arbitrary number of
-- children.

data Mtree a = Mnode a [Mtree a]

-- | showMtree returns a multi-line string depicting an Mtree. Each node
--   should be placed on a separate line. You can include \n in the string
--   for a newline. The children of a node should be placed on subsequent
--   lines, indented by one more space than the line giving the value in
--   the node.
showMtree :: Show a => Mtree a -> String
showMtree = showMtree' ""

showMtree' :: Show a => String -> Mtree a -> String
showMtree' indent (Mnode root children) =
    indent ++ show root ++ "\n" ++ concatMap (showMtree' (' ':indent)) children

-- Prelude> :l ShowMTree
-- [1 of 1] Compiling Main             ( ShowMTree.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main> putStr $ showMtree (Mnode 1 [Mnode 2 [], Mnode 3 [Mnode 4 []]])
-- 1
--  2
--  3
--   4