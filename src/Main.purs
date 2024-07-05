module Main
  ( Board
  , Cell(..)
  , Player(..)
  , anyMovesLeft
  , checkWinner
  , isGameOver
  , mkBoard
  ) where

import Prelude

import Data.Array (all, any, concatMap, index, length, mapWithIndex, transpose, (..))
import Data.Array.Partial (head, last)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Partial.Unsafe (unsafePartial)

data Cell
  = Empty
  | XSymbol
  | OSymbol

derive instance Eq Cell

data Player
  = XPlayer
  | OPlayer

derive instance Eq Player
instance Show Player where
  show = case _ of
    XPlayer -> "XPlayer"
    OPlayer -> "OPlayer"

-- it's assumed that any given Board can be reached by traditional gameplay of tic-tac-toe
--   (i.e. a player can only win on their turn)
newtype Board = Board (Array (Array Cell))

type Selector = Board -> Array (Array Cell)

mkBoard :: Array (Array Cell) -> Maybe Board
mkBoard cells =
  let
    height = length cells
    isSquare = all (eq height <<< length) cells
  in
    if isSquare && height >= 2 then Just $ Board cells
    else Nothing

anyMovesLeft :: Board -> Boolean
anyMovesLeft (Board cells) = any (any (eq Empty)) cells

isGameOver :: Board -> Boolean
isGameOver = isJust <<< checkWinner || not anyMovesLeft

checkWinner :: Board -> Maybe Player
checkWinner board =
  let
    selectors = [ rows, columns, diagonals, fourCorners, twoByTwoBoxes ]
    possibleWins = concatMap (\selector -> selector board) selectors
    allMatchPlayer player = all (eq (Just player) <<< cellPlayer)
    hasWon player = any (allMatchPlayer player) possibleWins
  in
    if hasWon XPlayer then Just XPlayer
    else if hasWon OPlayer then Just OPlayer
    else Nothing

rows :: Selector
rows (Board cells) = cells

columns :: Selector
columns (Board cells) = transpose cells

diagonals :: Selector
diagonals (Board cells) =
  let
    height = length cells
  in
    [ mapWithIndex (\i _ -> unsafeIndex2 cells i i) cells -- backward diagonal
    , mapWithIndex (\i _ -> unsafeIndex2 cells (height - i - 1) i) cells -- forward diagonal
    ]

fourCorners :: Selector
fourCorners (Board cells) =
  let
    firstRow = unsafeHead cells
    lastRow = unsafeLast cells
  in
    [ [ unsafeHead firstRow
      , unsafeLast firstRow
      , unsafeHead lastRow
      , unsafeLast lastRow
      ]
    ]

twoByTwoBoxes :: Selector
twoByTwoBoxes (Board cells) =
  let
    height = length cells

    getBoxFromTopLeft :: Int -> Int -> Array Cell
    getBoxFromTopLeft x y =
      [ unsafeIndex2 cells x y
      , unsafeIndex2 cells (x + 1) y
      , unsafeIndex2 cells x (y + 1)
      , unsafeIndex2 cells (x + 1) (y + 1)
      ]
  in
    concatMap -- get 2x2 boxes using all of the "top left" cells (excluding bottom row and last column)

      ( \x ->
          map (getBoxFromTopLeft x) (0 .. (height - 2))
      )
      (0 .. (height - 2))

cellPlayer :: Cell -> Maybe Player
cellPlayer = case _ of
  Empty -> Nothing
  XSymbol -> Just XPlayer
  OSymbol -> Just OPlayer

unsafeHead :: forall a. Array a -> a
unsafeHead arr = unsafePartial $ head arr

unsafeLast :: forall a. Array a -> a
unsafeLast arr = unsafePartial $ last arr

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex arr i = unsafePartial $ fromJust $ index arr i

unsafeIndex2 :: forall a. Array (Array a) -> Int -> Int -> a
unsafeIndex2 arr x y = unsafeIndex (unsafeIndex arr y) x