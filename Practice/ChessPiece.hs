data ChessPiece = ChessPiece Colour Rank

data Colour = Black | White

data Rank = King | Queen | Rook | Bishop | Knight | Pawn

instance Show ChessPiece where
    show (ChessPiece c r) = show c ++ show r

instance Show Colour where
    show Black = "B"
    show White = "W"

instance Show Rank where
    show King   = "K"
    show Queen  = "Q"
    show Rook   = "R"
    show Bishop = "B"
    show Knight = "N"
    show Pawn   = "P"

toChessPiece :: String -> Maybe ChessPiece
toChessPiece str =
    case filter (/=' ') str of
        [c,r] -> do
            colour <- toColour c
            rank   <- toRank r
            return $ ChessPiece colour rank
        _ -> Nothing

toColour :: Char -> Maybe Colour
toColour 'B' = Just Black
toColour 'W' = Just White
toColour _   = Nothing

toRank :: Char -> Maybe Rank
toRank 'K' = Just King
toRank 'Q' = Just Queen
toRank 'R' = Just Rook
toRank 'B' = Just Bishop
toRank 'N' = Just Knight
toRank 'P' = Just Pawn
toRank _   = Nothing