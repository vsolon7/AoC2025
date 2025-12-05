{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ClosedInterval = ClosedInterval
  { leftE :: Integer
  , rightE :: Integer
  }
    deriving Eq

instance Show ClosedInterval where
  show (ClosedInterval leftE rightE) = "[" ++ show leftE ++ "," ++ show rightE ++ "]"

{- [a,b] <= [c,d] if either a <= c, or a = c and b >= d.
 - i.e., either [a,b] lies (not necessarily strictly) to the left of [c,d] or [a,b] contains [c,d].
 - This is sort of a hack for the set lookup later on in the fresh function.
 -}
instance Ord ClosedInterval where
  compare i i' =
    case compare (leftE i) (leftE i') of
      LT -> LT
      GT -> GT
      EQ ->
        case compare (rightE i) (rightE i') of
          LT -> GT
          GT -> LT
          EQ -> EQ

readT = read . T.unpack

{- Returns Nothing if the intervals are disjoint, otherwise their union.
 - Assumes the left endpoint of i is <= the left endpoint of i', for simplicity (this is the only case we need).
-}
combine :: ClosedInterval -> ClosedInterval -> Maybe ClosedInterval
combine i i' =
  if rightE i < leftE i'
    then Nothing
    else Just $ ClosedInterval (leftE i) (max (rightE i) (rightE i'))

parseInput :: FilePath -> IO ([ClosedInterval], [Integer])
parseInput file = do
  file <- TIO.readFile file
  let (strintervals, _:strints) = break T.null (T.lines file)
  let ints = map readT strints
  let intervals = map ((\[x,y] -> ClosedInterval (readT x) (readT y))
                       . (T.splitOn "-")
                      )
                      strintervals
  return (intervals, ints)

{- Given a list of intervals sorted via the left endpoint, return the connected components of the list of intervals.
 - The result will be a list of disjoint intervals, sorted in reverse orer (since they're disjoint, sorted by their
 - left endpoint is the same as sorted by their right endpoint).
-}
connectedCmpnts :: [ClosedInterval] -> [ClosedInterval]
connectedCmpnts [] = []
connectedCmpnts (i:is) = go is i []
  where
    -- currComp is the current connected component
    go [] currComp total = currComp : total
    go (i:is) currComp total =
      case (combine currComp i) of
        Nothing  -> go is i (currComp : total) -- intervals are disjoint, so our connected component is done
        Just c' -> go is c' total

-- S.Set ClosedInterval should be a set of DISJOINT intervals for this to work
isFresh :: S.Set ClosedInterval -> Integer -> Bool
isFresh set x =
  {- n is in the interval [a,b] if and only if [a,b] <= [n,n] in the ordering defined above.
   - Since the intervals in the set are disjoint, n is in at most one interval, 
   - so we just check the "best candidate." -}
  case (S.lookupLE (ClosedInterval x x) set) of
    Nothing -> False
    Just i  -> if x <= rightE i then True else False

main :: IO ()
main = do
  (intervals,ints) <- parseInput "input.txt"

  {- Since connectedCmpnts gives a list sorted in reverse order, I use S.fromList. I thought I would be able
   - to use S.fromDescList, but I seem to have confused myself -}
  let disjointSet = S.fromList . connectedCmpnts . sort $ intervals
  let one = length . filter id . map (isFresh disjointSet) $ ints

  {- Part two just folds the set by accumulating the total number of elements in each interval. 
   - The number of elements in a closed interval [a,b] is b - a + 1. (!) -}
  let two = S.foldr'
              (\i acc ->
                let numElms = rightE i - leftE i + 1 in
                  acc + numElms
              )
              0
              disjointSet

  putStrLn $ "Part 1: " ++ show one
  putStrLn $ "Part 2: " ++ show two
