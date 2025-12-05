{-# LANGUAGE DerivingVia #-}
module Main (main) where

import Data.PSQueue as PSQ
import Data.Array.IArray
import Data.Monoid
import Data.Ix

newtype Pos = Pos (Int, Int)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Ix)
  deriving (Semigroup, Monoid) via (Sum Int, Sum Int)

-- test grid : 10 x 10, real grid : 137 x 137
-- obviously, there is a better way to do this
_HEIGHT = 137
_WIDTH = 137

neighborInds:: Eq a => PSQ Pos a -> Pos -> [Pos]
neighborInds psq pos = filter (\k -> PSQ.lookup k psq /= Nothing) indices
  where
    neighborSquare = filter (/= Pos (0,0)) [Pos (i,j) | i <- [-1,0,1], j <- [-1,0,1]]
    indices = map (pos <>) neighborSquare

numAdjacentRolls :: Array Pos Char -> Pos -> Maybe Int
numAdjacentRolls arr pos = case (arr ! pos) of
  '.' -> Nothing -- we only care about the paper rolls surrounding a paper roll
  _   -> Just (length . filter (== Just '@') $ neighbors)
    where
      neighborSquare = filter (/= Pos (0,0)) [Pos (i,j) | i <- [-1,0,1], j <- [-1,0,1]]
      neighbors = map ((arr !?) . (pos <>)) neighborSquare

inputToPSQ :: FilePath -> IO (PSQ Pos Int)
inputToPSQ file = do
  text <- readFile file
  let flattened = filter (/= '\n') text

  -- go through an intermediate array
  let grid = listArray (Pos (0,0), Pos (_HEIGHT - 1,_WIDTH - 1)) flattened :: Array Pos Char
  return $
    Prelude.foldr
      (\pos psq -> case (numAdjacentRolls grid pos) of
        Nothing -> psq
        Just n  -> insert pos n psq)
      empty
      (range . bounds $ grid)

removeLoneliestRoll :: PSQ Pos Int -> PSQ Pos Int
removeLoneliestRoll psq =
  case findMin psq of
    Nothing -> psq
    Just loneliest -> let newpsq = deleteMin psq in
      Prelude.foldr
        (\pos lpsq -> adjust (\p -> p - 1) pos lpsq)
        newpsq
        (neighborInds newpsq (key loneliest))

numRemovals :: PSQ Pos Int -> Int
numRemovals psq = go psq 0
  where
    go :: PSQ Pos Int -> Int -> Int
    go psq' n = case findMin psq' of
      Nothing -> n
      Just b  ->
        if prio b > 3 then n
        else go (removeLoneliestRoll psq') (n+1)

main :: IO ()
main = do
  psq <- inputToPSQ "input.txt"
  putStrLn $
    "Part 2: " ++ show (numRemovals psq)
