{-# LANGUAGE DerivingVia #-}

import Data.Monoid
import Data.Array.IArray

_gHEIGHT = 137
_gWIDTH = 137

_tgHEIGHT = 10
_tgWIDTH = 10

newtype Pos = Pos (Int, Int)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Ix)
  deriving (Semigroup, Monoid) via (Sum Int, Sum Int)

data Grid i a = Grid
  { focus :: i
  , contents :: Array i a }
    deriving Show

instance Functor (Grid i) where
  fmap f grid = Grid (focus grid) (fmap f (contents grid))

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  -- since fmap f :: w (w a) -> w b, fmap f . duplicate is given by w a |-> w (w a) |-> w b

instance (Ix i, Monoid i) => Comonad (Grid i) where
  -- extract the focus point
  extract grid = contents grid ! (focus grid)
  
  -- make a grid of grids. the outer grid has focus equal to the focus of the original grid.
  -- each inner grid at position (i,j) has focus at (i,j)
  duplicate grid = Grid
                { focus = focus grid
                , contents = genArray (bounds c) $ \i -> 
                               Grid
                               { focus = i <> (focus grid)
                               , contents = c }
                }
                  where c = contents grid

inputToGrid :: FilePath -> IO (Grid Pos Char)
inputToGrid file = do
  text <- readFile file
  let flattened = filter (/= '\n') text
  return $
    Grid
    { focus = Pos (0,0)
    , contents = listArray (Pos (0,0), Pos (_gHEIGHT - 1,_gWIDTH - 1)) flattened }

accessible :: Grid Pos Char -> Maybe Bool
accessible g@(Grid focus contents) = case (extract g) of
  '.' -> Nothing
  _   -> if (numPapers <= 4) then (Just True) else (Just False)
    where
      neighbors = [contents !? (focus <> Pos (i,j)) | i <- [-1,0,1], j <- [-1,0,1]]
      numPapers = length . filter (== (Just '@')) $ neighbors

countAccessible :: Grid Pos (Maybe Bool) -> Int
countAccessible grid =
  foldrArray'
    (\b acc ->
      if b == Just True then acc + 1
      else acc)
    0
    (contents grid)

main :: IO ()
main = do
  grid <- inputToGrid "input.txt"
  print $
    countAccessible . extend accessible $ grid
