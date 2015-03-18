{-# LANGUAGE OverloadedStrings #-}

{-
some kind of chess thingy
-}

import Data.Array.IArray
import Data.List (intersperse, groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Text (strip)
import Text.Read (readMaybe)


data Rank = Knight | Rook | Bishop | Pawn | King | Queen | Turd
          deriving (Show, Eq)
data Colour = White | Black
            deriving (Show, Eq)

data Piece = P Colour Rank | None 
           deriving Eq 


-- | a game concists of a board and whos turn it is
data Game = G Board Colour


-- maybe im needing later
                                      
instance Show Piece where
  show (P _ Turd) = "💩" 
  show None  = " "
  show (P colour rank) = case colour of
    Black -> case rank of
      Knight -> "♞"
      Rook   -> "♜"
      Bishop -> "♝"
      Pawn   -> "♟"
      King   -> "♚"
      Queen  -> "♛"
    White -> case rank of
      Knight -> "♘"
      Rook   -> "♖"
      Bishop -> "♗"
      Pawn   -> "♙"
      King   -> "♔"
      Queen  -> "♕"

data Tile = T {
  color :: Colour,
  pos   :: TPos,
  piece :: Piece 
              }
            
instance Show Tile where
  show = show . piece

type TPos = (Char, Int)
data Move = M TPos TPos
          deriving Show
                     

getPiece :: Board -> TPos -> Piece
getPiece (B board) pos = piece $ board ! pos

-- TODO: make a ruleset
validMove :: Move -> Bool
validMove (M p p') = undefined

strToMove :: String -> Maybe Move
strToMove s = case splitOn " " s of
  [p1, _to, p2] -> case (str2p p1, str2p p2) of
    (Just p1', Just p2') -> Just $ M p1' p2'
    _                    -> Nothing 
    where
      str2p :: String -> Maybe TPos
      str2p  (c:i) = case readMaybe i :: Maybe Int of
        Just i' -> if validPos (c, i')
                   then Just (c, i')
                   else Nothing
        Nothing -> Nothing

    
validPos :: TPos -> Bool
validPos (c,i) = c >= 'A' && c <= 'H' && i >= 1 && i <= 8

startPiece :: TPos -> Piece
startPiece (ch, i) = case i of
  1 -> P White $ lineup ch
  2 -> P White Pawn
  7 -> P Black Pawn
  8 -> P Black $ lineup ch
  _ -> if i >= 3 && i <= 6 then None
       else error $ "Out of boards at " ++ show ch ++ show i
  where lineup c | elem c "AH" = Rook
                 | elem c "BG" = Knight
                 | elem c "CF" = Bishop
                 | c == 'D'    = King
                 | c == 'E'    = Queen
                 | otherwise   = error $ "Out of boards at "
                                 ++ show ch ++ show i
                                 
          
  
 
setPiece :: Tile -> Piece -> Tile 
setPiece t p = t {piece = p}

newTile :: TPos -> Tile
newTile p = T { color = col p,
                pos   = p,
                piece = startPiece p}
  where
    evenCh ch = ch `elem` "BDFH"
    col (ch, i) | evenCh ch `xor` even i = Black
                | otherwise              = White
    xor a b = (a || b) && (not $ a && b)

newtype Board = B (Array (Char, Int) Tile)

instance Show Board where
  show (B bard) = box . map (concatMap pp) . groupsort $ bard
    where 
      groupsort = groupBy (\t1 t2 -> i t1 == i t2)
            . sortBy (\t1 t2 -> i t1 `compare` i t2 )
            . assocs

      i ((_,i'),_) = i'
      pp ((c, i), tile) = show $ piece tile

newBoard :: Board
newBoard = B $ array (('A', 1), ('H', 8))
        [(pos, newTile pos) | pos <- boardRange]
  where boardRange = [(ch, i) | ch <- ['A'..'H'], i <- [1..8]]



{-
Translates, for example

> putStr $ show ["hej", "du", "glade"]
["hej", "du", "glade"]
to

> putStr $ box ["hej", "du", "glade"]
╒═╤═╤═╤═╤═╕
│h│e│j│ │ │
├─┼─┼─┼─┼─┤
│d│u│ │ │ │
├─┼─┼─┼─┼─┤
│g│l│a│d│e│
╘═╧═╧═╧═╧═╛

-}

box :: [String] -> String
box ss | len < 0   = error "can't make an empty box!" 
       | otherwise = topLine
                     ++ (concat $ intersperse divider $ map row' ss)
                     ++ bottomLine
  where
    len = (maximum $ map length ss) - 1
    liney pre line cross suf 
      = (++ "\n") $ intersperse line $ pre ++ replicate len cross ++ suf

    strLine pre str line suf
      = (++ "\n") $ pre ++ (intersperse line str) ++ suf

    row' str | length str < (len+1) = row $ str
                                      ++ (replicate (len + 1 - length str) ' ')
             | otherwise = row str 
    topLine    = liney   "╒" '═' '╤' "╕"
    row str    = strLine "│" str '│' "│"
    divider    = liney   "├" '─' '┼' "┤"
    bottomLine = liney   "╘" '═' '╧' "╛" 

              
