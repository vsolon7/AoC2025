{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- These are closed intervals
data Interval = Interval
  { leftE :: Integer
  , rightE :: Integer
  }
    deriving Eq

instance Show Interval where
  show (Interval leftE rightE) = "[" ++ show leftE ++ "," ++ show rightE ++ "]"

{- The ordering is: [a,b] <= [c,d] if either c <= a, or a = c and b <= d.
 - i.e., [a,b] <= [c,d] if either [c,d] is "to the left" of [a,b], or a = c and [a,b] is contained in [c,d].
 -
 - The ordering is defined this way because later we will make a Set of intervals. Querying the set will perform a
 - binary search, and the code ends up nicer than if I just used the dictionary ordering. It's essentially a
 - hack to avoid writing my own binary search algorithm on my own binary trees.
 -
 - You can think of this ordering as a relaxation of containment, where <= now means "the first endpoint satisfies
 - the condition implied by containment." In the case of equal first endpoints, we actually do have containment.
 -}
instance Ord Interval where
  compare i i' =
    case compare (leftE i) (leftE i') of
      LT -> GT
      GT -> LT
      EQ ->
        case compare (rightE i) (rightE i') of
          LT -> LT
          GT -> GT
          EQ -> EQ

readT = read . T.unpack

{- Sort by the opposite ordering defined above, thus sorting the intervals by their left endpoints.
 -
 - This is somewhat unfortunate; the alternative is defining the ordering above to be the opposite ordering and sorting
 - normally. If you do that, you find:
 -   - [-oo, oo] is the smallest interval
 -   - [oo,oo] is the largest interval
 -   - [5,100] <= [5,6]
 - which feels exactly backwards from what <= "should" mean.
 -}
sortByLeft :: [Interval] -> [Interval]
sortByLeft = sortBy (\i i' -> compare (Down i) (Down i'))

{- Returns Nothing if the intervals are disjoint, otherwise their union.
 - Assumes the left endpoint of i is <= the left endpoint of i', for simplicity (this is the only case we need).
-}
combine :: Interval -> Interval -> Maybe Interval
combine i i' =
  if rightE i < leftE i'
    then Nothing
    else Just $ Interval (leftE i) (max (rightE i) (rightE i'))

parseInput :: FilePath -> IO ([Interval], [Integer])
parseInput file = do
  input <- TIO.readFile file
  let (strintervals, _:strints) = break T.null (T.lines input)
  let ints = map readT strints
  let intervals = map ((\[x,y] -> Interval (readT x) (readT y))
                       . (T.splitOn "-")
                      )
                      strintervals
  return (intervals, ints)

{- Given a list of intervals sorted via the left endpoint, return the connected components of the list of intervals.
 - The result will be a list of disjoint intervals, sorted in reverse orer (since they're disjoint, sorted by their
 - left endpoint is the same as sorted by their right endpoint).
-}
connectedCmpnts :: [Interval] -> [Interval]
connectedCmpnts [] = []
connectedCmpnts (i:is) = go is i []
  where
    -- currComp is the current connected component
    go [] currComp total = currComp : total
    go (j:js) currComp total =
      case (combine currComp j) of
        Nothing -> go js j (currComp : total) -- intervals are disjoint, so our connected component is done
        Just c' -> go js c' total

{- S.Set Interval must be a set of DISJOINT intervals for this to work.
 - 
 - Note that n is in the interval [a,b] if and only if [n,n] <= [a,b] in the ordering defined above.
 - Since the intervals in the set are disjoint, n is in at most one interval. We need only check the "best candidate."
 -}
isFresh :: S.Set Interval -> Integer -> Bool
isFresh set x =
  case (S.lookupGE (Interval x x) set) of
    Nothing -> False
    Just i  -> if x <= rightE i then True else False

main :: IO ()
main = do
  (intervals,ints) <- parseInput "input.txt"

  {- sortByLeft sorts via the opposite ordering defined on the intervals, and connectedCmpnts returns the components
   - in a list sorted in the opposite opposite ordering; i.e., the original ordering. Therefore, we use fromAscList.
   -}
  let disjointSet = S.fromAscList . connectedCmpnts . sortByLeft $ intervals
  let one = length . filter id . map (isFresh disjointSet) $ ints

  {- Just fold the set by accumulating the number of elements in each disjoint interval.
   - The number of elements in a closed interval [a,b] is b - a + 1. (!)
   -}
  let two = S.foldr'
              (\i acc ->
                let numElms = rightE i - leftE i + 1 in
                  acc + numElms
              )
              0
              disjointSet

  putStrLn $ "Part 1: " ++ show one
  putStrLn $ "Part 2: " ++ show two
