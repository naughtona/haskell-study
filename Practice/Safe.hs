data Square = Red | Black | Empty deriving (Eq, Show)

-- | safe takes a board in the form of a list of lists of Squares and returns
--   True if no Square contains a Red where there is a Black square diagonally
--   adjacent to it in the following row
safe :: [[Square]] -> Bool
safe [] = True
safe [_] = True
safe (a1:a2:as)
    | safePair a1 a2 = safe (a2:as)
    | otherwise = False

safePair :: [Square] -> [Square] -> Bool
safePair as bs = not lAttack && null mAttack && not rAttack
    where mAttack = filter (\(a,b) -> a==Red && b==Black) $ zip as $ tail bs
          lAttack = (as !! 1)==Red && (head bs)==Black
          rAttack = (last as)==Red && ((reverse bs) !! 1)==Black


-- Prelude> :l Safe
-- [1 of 1] Compiling Main             ( Safe.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main> safe [[Red,Empty],[Empty,Black]]
-- False
-- *Main> safe [[Black,Empty],[Empty,Red]]
-- True
-- *Main> safe [[Red,Black],[Red,Red],[Black,Empty]]
-- False
-- *Main> safe [[Empty,Black,Red],[Red,Black,Empty]]
-- False