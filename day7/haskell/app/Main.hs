import Control.Monad
import qualified Data.Map as M

{- The idea is to attach to every beam a "beam multiplicity" that describes the number of beams that occupy that same
 -  space after splitting. The number of paths is just the sum of the beam multiplicities of the last row (you can
 -  also think about Pascal's triangle and how the number at a position gives you the number of paths from the top to
 -  that position).
 -
 -  Therefore, we use a Map Int Int. The keys of the map are the positions of the beams in each row, and the
 -  value associated to any key is the current multiplicity. We have a list of maps because we compute one row at a time.
 -  The only snag is that the locations of the splitters are also given by a map because we can't intersect a set and a
 -  map. We must to attach some value to each splitter, then, so we attach the value of 1 to make things more
 -  convenient later (the Map for the first row also gives the Map for the starting beam, with multiplicity 1).
 -
 -  Note that the parsed input discards the first line, as well as all of the lines that contain only the '.' character.
 -}
parseInput :: String -> [M.Map Int Int]
parseInput text = do
  line <- lines text
  let s = M.fromAscList ([(n,1) | (n,'^') <- zip [0..] line])
  guard (not . M.null $ s)
  return s

{- Process one row.
 -
 - Given:
 -   The locations of the splitters in this row,
 -   the beams with multiplicities coming from the previous row,
 -   the total number of splits that have taken place so far,
 -
 - Return:
 -   The beams with multiplicities for the next row, as well as the new total number of splits.
-}
split :: M.Map Int Int -> (M.Map Int Int, Int) -> (M.Map Int Int, Int)
split splitters (beamMults, totalSplits) =
  let
    toSplit = M.intersection beamMults splitters
    passingBeams = beamMults M.\\ toSplit
    splitBeams = M.unionWith (+) (M.mapKeysMonotonic (\n -> n - 1) toSplit) (M.mapKeysMonotonic (\n -> n + 1) toSplit)
    newSplits = M.size toSplit
  in
    (M.unionWith (+) splitBeams passingBeams, totalSplits + newSplits)

-- manually thread all the state through because i'm too dumb to use the state monad right now
runSplit :: [M.Map Int Int] -> (M.Map Int Int, Int) -> (M.Map Int Int, Int)
runSplit [] acc = acc
runSplit (x:xs) acc = runSplit xs (split x acc)

main :: IO ()
main = do
  inpt <- readFile "input.txt"
  let splitterList = parseInput inpt
  let startBeam = head splitterList
  let (finalRow,totalSplits) = runSplit splitterList (startBeam, 0)
  let totalPaths = M.foldl' (+) 0 finalRow
  putStrLn ("Part 1: " ++ show totalSplits)
  putStrLn ("Part 2: " ++ show totalPaths)
