{-
some kind of chess thingy
-}

import Data.Array.IArray
import Data.List

import qualified Data.Bool as B
data Rank = Knight | Rook | Bishop | Pawn | King | Queen | Turd
          deriving (Show, Eq)
data Colour = White | Black
            deriving (Show, Eq)

data Piece = P Colour Rank | None 
           deriving Eq 


-- maybe im needing later
pChar (P _ Turd) = '💩' 
pChar None  = ' '
pChar (P colour rank) = case colour of
  Black -> case rank of
    Knight -> '♞'
    Rook   -> '♜'
    Bishop -> '♝'
    Pawn   -> '♟'
    King   -> '♚'
    Queen  -> '♛'
  White -> case rank of
    Knight -> '♘'
    Rook   -> '♖'
    Bishop -> '♗'
    Pawn   -> '♙'
    King   -> '♔'
    Queen  -> '♕'
                                      
instance Show Piece where
  show = show . pChar 

data Tile = T {
  color :: Colour,
  pos   :: TPos,
  piece :: Piece 
              }
            -- deriving Show 
instance Show Tile where
  show = show . piece 


type TPos = (Char, Int)

validPos :: TPos -> Bool
validPos (c,i) = c >= 'A' && c <= 'G' && i >= 1 && i <= 8

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


box :: [String] -> String
box ss = topLine
         ++ (concat $ intersperse divider $ map row ss)
         ++ bottomLine
  where
    liney pre line cross suf
      = (++ "\n") $ intersperse line $ pre ++ replicate 7 cross ++ suf

    liney' pre str line suf
      = (++ "\n") $ pre ++ (intersperse line str) ++ suf

    topLine    = liney  "╒" '═' '╤' "╕"
    row str    = liney' "│" str '│' "│"
    divider    = liney  "├" '─' '┼' "┤"
    bottomLine = liney  "╘" '═' '╧' "╛" 


instance Show Board where
  show (B bard) = box . map (map pp) . groupsort $ bard
    where 
      groupsort = groupBy (\t1 t2 -> i t1 == i t2)
            . sortBy (\t1 t2 -> i t1 `compare` i t2 )
            . assocs

      i ((_,i'),_) = i'
      pp ((c, i), tile) = pChar $ piece tile


board :: Board
board = B $ array (('A', 1), ('H', 8))
        [(pos, newTile pos) | pos <- boardRange]
  where boardRange = [(ch, i) | ch <- ['A'..'H'], i <- [1..8]]

              
