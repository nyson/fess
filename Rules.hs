{-# LANGUAGE OverloadedStrings #-}

module Rules where

{-
some kind of chess thingy
-}

import Data.Array.IArray
import Data.List (groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Text (strip)
import Text.Read (readMaybe)
import Data.Tuple (swap)
import Data.Char (ord, chr, toLower)
import qualified Data.Function as Func (on)
import Data.Maybe
import Data.Default
import qualified Data.Map as Map 


import Control.Monad.Writer.Lazy

import Debug.Trace (trace)
import Position
import Stringboxer

newtype Board = B (Array TPos Tile)

instance Show Board where
  show (B board) = box . (:) (' ': ['1'..'8']) . mapWC (concatMap pp) ['A'..]
                  . groupsort $ board
    where
      groupsort = groupBy ((==) `Func.on` snd . fst)
            . sortBy (compare `Func.on` snd . fst)
            . assocs
      -- Map with a counter 
      mapWC f cs = map (\(a,b) -> a:f b) . zip cs
      pp ((c, i), tile) = case piece tile of
        None -> case colour tile of
          Black -> "■"
          White -> "□"
        p    -> show p


data Chess = Chess {
  board :: Board,
  turn  :: Colour,
  flags :: Map.Map (Colour, String) Bool,
  moves :: [Move] -- filo stack; move m, of moves = (m:ms), is always the
                  -- latest move 
  }

instance Show Chess where
  show b = "It's " ++ show (turn b) ++ "s turn and the game has so far had "
           ++ show (length $ moves b) ++ " moves.\n" ++ show (board b)

instance Default Chess where
  def = Chess {board= newBoard, turn= White, moves= [], flags = Map.empty }


-- Ranks of chess pieces
data Rank = Turd | King | Queen | Rook | Bishop | Knight | Pawn 
          deriving (Show, Eq)

-- Colour of chess pieces
data Colour = White | Black deriving (Show, Eq, Ord)
-- A piece                     
data Piece = P Colour Rank | None deriving Eq 


instance Show Piece where
  -- maybe im needing later
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
  colour :: Colour,
  pos    :: TPos,
  piece  :: Piece 
  }


instance Show Tile where
  show = show . piece

shlow :: Show a => a -> String
shlow = map toLower . show

data Move = M TPos TPos
          deriving Show

-- | Get piece on a certain tile of the board
getPiece :: Board -> TPos -> Piece
getPiece (B board) pos = piece $ board ! pos

-- | Predicate for position being on the board
inBoardRange :: TPos -> Bool
inBoardRange (a, b) = p a && p b
  where p i = i >= 1 && i <= 8  

{- TODO: make and check the ruleset

Data needed for decision
* Move:   Position to another Position
* Rank:   Knight | Rook | Bishop | Pawn | King | Queen | Turd
* Colour: White | Black

Legend: Verizontal = Horizontal or vertical

Common rules:
* Pieces, except the Knight, can't generally move across other pieces.
* Pieces can't land on friendly pieces.
* Pieces can't exist outside the board

Rules by rank:
* Knights can move 2 or 1 steps in one verizontal direction
   and then 1 or 2 steps steps orthogonal to the steps before
* Knights aren't blocked by allied pieces, but may not land
   on a friendly piece
* Rooks can move any steps in a verizontal direction
* Bishops can move any steps in a diagonal direction
* Queens can move as rooks and bishops
* Kings can move as queens but just one step
   unless they're doing a castling
* Pawns can move just one step, but diagonally if there's a piece to catch
   pawns cannot move forward if another piece is blocking the way.
* Pawns may move two steps forward on the first turn

Checking rules:
* A king is put in check when it may be taken by an opposing piece
   the next turn.
* When a king is in check, it may only do moves that removes the threat
   by either
** Block the threat
** Remove the threat
** Move to a non-threatened space
* If none of the above is possible, it's a check mate and the opposing player
   has won the game.

Castling rules:
* Only a king may perform a castling on a rook
* Pieces affected by the castling may not have moved before
* No pieces may stand between the king and the rook
* The king may not currently be in check, land in check or pass any tile
   that would put them in check


__En Passant Rule__
╒═╤═╕    ╒═╤═╕    ╒═╤═╕
│♟│ │    │♟│ │    │♟│ │
├─┼─┤    ├─┼─┤    ├─┼─┤
│ │♟│ => │♙│♟│ => │ │ │
├─┼─┤    ├─┼─┤    ├─┼─┤
│ │ │    │ │ │    │♟│ │
├─┼─┤    ├─┼─┤    ├─┼─┤
│♙│ │    │ │ │    │ │ │
╘═╧═╛    ╘═╧═╛    ╘═╧═╛

En passant:
* A pawn that moved two steps may be a victim of an en passant as
   the opposing player may the next turn attack the piece by moving to
   the tile between the players two positions. This move is only valid the
   turn after the pawn victim moved.

Pawn promotion:
* A pawn reaching the eight rank (the tile row at the end of the board)
   may be promoted to another piece. The piece may be converted to a queen,
   bishop, rook or knight of the same colour. There is no limit on how many
   pieces of a kind there may be. 
-}

-- type Step = (Int, Int)
-- type WalkPredicate = TPos -> Step -> Bool
    

movesOf :: Chess -> TPos -> [Move]
movesOf s pos@(x,y) = wrap $ case getPiece (board s) pos of
  None       -> []
  P col rank -> case rank of
    Turd   -> [(x',y') | x' <- [1..8], y' <- [1..8]]
    
    Knight -> let incrs = [1,2,-1,-2]
              in filter (\p -> not $ ownerEq s p col)
                 [(x+x',y+y') | x' <- incrs, y' <- incrs, abs y' /= abs x']
              
    Bishop -> takeWhileAhead (pred col)
              $ steps (anyway $ Incs [(1, 1)]) pos

    Rook   -> takeWhileAhead (pred col)
              $ steps (anyway $ Incs [(1, 0), (0,1)]) pos
              
    Queen  -> takeWhileAhead (pred col)
              $ steps (anyway $ Incs [(1,0), (0,1), (1,1)]) pos
              
    King   ->
      let kingHasMoved = const False
          hasBeenChecked = False
      in execWriter $ do
        -- Basic king movement 
        tell [(a', b') | a' <- [x-1..x+1], b' <- [y-1..y+1], (a', b') /= (x, y)]
        -- Castling
        tell []
      
              
    Pawn   ->
      let (start, inc) = setupPawn col
          enemy = opponent col
          (oppStart, oInc) = setupPawn enemy
          hasEnemy ep = ownerEq s ep enemy
          pawnThatMovedLastTurn p = case head $ moves s of
            M from to@(a,b) | p == to && ownerEq s to enemy
                              -> from == (a, oppStart)
            _                 -> False
                  
      in execWriter $ do
        -- Basic movement
        tell [(x, y `inc` 1)]
        -- May move one more step if it's in starting position
        tell $ if start /= y then [] else [(x, y `inc` 2)]

        -- May move diagonally if there's an enemy piece there
        tell $ filter hasEnemy [(x + i, y `inc` 1) | i <- [-1, 1]]
                
        -- Enforceing enpassant rules 
        tell
          $ map (\(a,b) -> (a, b `inc` 1))
          $ filter pawnThatMovedLastTurn [(x + i, y) | i <- [-1, 1]]
  where
    -- Final wrap of the move check
    wrap = map (M pos) . filter inBoardRange
    
    -- | takeWhile but with the predicate of the element tupled with its
    -- |  lookahead 
    takeWhileAhead p xs = map fst $ takeWhile p (zip xs $ tail xs)

    -- | predicate for walking pieces like rook, queen and bishop
    pred col (position, next) = inBoardRange position

    setupPawn Black = (2, (+))
    setupPawn White = (7, (-))

checked :: Colour -> Board -> Bool
checked = undefined

opponent :: Colour -> Colour
opponent White = Black
opponent Black = White

owner :: Board -> TPos -> Maybe Colour
owner (B b) p = if inBoardRange p then
                  case piece $ b ! p of
                    None  -> Nothing
                    P a _ -> Just a
                else Nothing

ownerEq :: Chess -> TPos -> Colour -> Bool
ownerEq c p col = case owner (board c) p of
  Just col' -> col == col'
  Nothing   -> False

-- | Moves a piece on the board, should only be done after validMove
movePiece :: Board -> Move -> Board
movePiece (B board) (M p p') = undefined 

strToMove :: String -> Maybe Move
strToMove s = case splitOn " " s of
  [p1, _to, p2] -> do
    p1' <- str2p p1
    p2' <- str2p p2
    return $ M p1' p2'
      where
        stripParens = filter $ \x -> x `notElem` "()"
        str2p str = case splitOn "," . stripParens $ str of
          [c,i] -> do
            c' <- readMaybe c
            i' <- readMaybe i
            return (c', i')
          _ -> Nothing
  _ -> Nothing 

    
newTile :: TPos -> Tile
newTile p = T { colour = col p,
                pos    = p,
                piece  = startPiece p}
  where
    col (ch, i) | even ch `xor` even i = Black
                | otherwise              = White
    xor a b = (a || b) && not (a && b)
    startPiece :: TPos -> Piece
    startPiece (ch, i) = case i of
      1 -> P White $ lineup ch
      2 -> P White Pawn
      7 -> P Black Pawn
      8 -> P Black $ lineup ch
      _ -> if i >= 3 && i <= 6 then None
           else error $ "Out of boards at " ++ show ch ++ show i
                
    lineup c | c `elem` [1,8] = Rook
             | c `elem` [2,7] = Knight
             | c `elem` [3,6] = Bishop
             | c == 5         = King
             | c == 4         = Queen


newBoard :: Board
newBoard = B $ array ((1, 1), (8, 8))
        [(pos, newTile pos) | pos <- boardRange]
  where boardRange = [(ch, i) | ch <- [1..8], i <- [1..8]]


