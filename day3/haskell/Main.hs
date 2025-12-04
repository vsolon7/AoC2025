import Data.List
import Data.Char

-- abstract zipper stuff
data Zipper a = Zipper [a] a [a]
  deriving Show

zright :: Zipper a -> Zipper a
zright z@(Zipper ls x []) = z
zright (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

toZipper :: [a] -> Zipper a
toZipper [] = error "empty zipper"
toZipper (x:xs) = Zipper [] x xs

fromZipper :: Zipper a -> [a]
fromZipper (Zipper ls x rs) = reverse ls ++ [x] ++ rs

parseInput :: FilePath -> IO [[Int]]
parseInput file = do
  input <- readFile file
  return $
    map (map digitToInt) . lines $ input

-- remove the index of the first valley point starting from the focus point of the zipper, or the last
-- element if no valley point exists.
-- a valley point in a list of ordered elements is an element that is strictly smaller than the adjacent element
-- for example, in 5421234215, the first valley point is the first 1
-- in 56321111, there are no valley points, so the final one is removed. in 2345, the first valley point is 2.
-- returns a zipper focused on the index immediately before the one removed.
-- this function solves the problem when the length of the bank is exactly one more than the maximum number of
-- batteries you can turn on; the zipper returned will be the answer, once we convert it to a list then an integer.
removeValley :: Ord a => Zipper a -> Zipper a
removeValley (Zipper [] x []) = Zipper [] x []
removeValley (Zipper (l:ls) x []) =
  if x > l then Zipper ls x []
    else Zipper ls l []
removeValley zxs@(Zipper [] x (r:rs)) =
  if r > x then Zipper [] r rs
    else removeValley . zright $ zxs
removeValley zxs@(Zipper (l:ls) x (r:rs)) =
  if r > x then Zipper ls l (r:rs)
    else removeValley . zright $ zxs

-- load a zipper with the first n elements of a list
load :: Int -> [a] -> (Zipper a, [a])
load n xs = let (first,last) = splitAt n xs
            in (toZipper $ first, last)

maxjoltage' :: Ord a => Int -> [a] -> [a]
maxjoltage' n xs = go (load n xs)
  where
    -- have to pay a cost of 1 traversal plus a partial reversal to convert from zipper to list
    go (zxs,[]) = fromZipper zxs

    -- ++ is inefficient, appending to the end of a zipper is bad.
    go ((Zipper ls x rs),(y:ys)) = go (removeValley (Zipper ls x (rs ++ [y])),ys)

-- i.e., [1,2,3] becomes 123. the reversal could be optimized away.
listToInt :: [Int] -> Integer
listToInt ds = sum [toInteger (10^k * d) | (k,d) <- zip [0..] (reverse ds)]

main :: IO ()
main = do
  banks <- parseInput "input.txt"
  putStrLn $
    "Part 1: " ++
    show (sum . map (listToInt . (maxjoltage' 2)) $ banks)
  putStrLn $
    "Part 2: " ++
    show (sum . map (listToInt . (maxjoltage' 12)) $ banks)
