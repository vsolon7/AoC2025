import Data.List
import Data.Char

-- Zippers!
data Zipper a = Zipper [a] a [a]
  deriving Show

zleft z@(Zipper [] x rs) = z
zleft (Zipper (l:ls) x rs) = Zipper ls l (x:rs)

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

{- Remove the index of the first valley point starting from the focus point of the zipper, or the last element if no
 - valley point exists. A valley point in a list of ordered elements is an element that is strictly smaller than the
 - adjacent element.
 -
 - EXAMPLES: In 5421234215, the first valley point is the first 1.
 -           In 56321111, there are no valley points, so the final 1 is removed.
 -           In 2345, the first valley point is 2.
 -
 - Returns a zipper focused on the index immediately before the one removed.
 -
 - This function solves the problem when the length of the bank is exactly one more than the maximum number of
 - batteries you can turn on; the zipper returned will be the answer, once we convert it to a list then an integer.
 -}
removeValley :: Ord a => Zipper a -> Zipper a
removeValley (Zipper [] x []) = Zipper [] x []
removeValley (Zipper (l:ls) x [])
  | x > l = Zipper ls x []
  | otherwise = Zipper ls l []
removeValley zxs@(Zipper ls x (r:rs))
  -- r may become a peak after x is removed, so we move left by one unit
  -- note: zleft (Zipper [] x rs) is just the identity, so this is not unsafe.
  | r > x = zleft (Zipper ls r rs)
  | otherwise = removeValley (zright zxs)

-- load a zipper with the first n elements of a list
load :: Int -> [a] -> (Zipper a, [a])
load n xs = let (first,last) = splitAt n xs in (toZipper $ first, last)

{- Find the max joltage a bank can produce, where we are turning on n batteries.
 - This function loads the first n elements of the bank into a zipper, then does the following:
 - 1. Adjoin the next element of the list to the zipper, to have a zipper of n+1 elements.
 - 2. Apply removeValley to the zipper, which solves the problem for a bank of n+1 batteries.
 - 3. Keep doing this until we run out of batteries in the bank.
 -
 - The magic is that repeatedly solving the case where the bank has n+1 batteries in fact solves the problem.
 -
 - The function could be optimized to remove the ++, and the use of the load function. One can feed the entire bank
 - into the zipper to begin with, then keep track of what position you are at in the zipper. When we get to the (n+1)th
 - element, we treat it as if we are at the end of the zipper in the removeValley algorithm above. Doing this requires
 - more variables, and so hurts code understanding, but is more optimal.
 -}
maxjoltage :: Ord a => Int -> [a] -> [a]
maxjoltage n xs = go (load n xs)
  where
    -- have to pay a cost of 1 traversal to convert from zipper to list
    go (zxs,[]) = fromZipper zxs

    -- ++ is inefficient
    go ((Zipper ls x rs),(y:ys)) = go (removeValley (Zipper ls x (rs ++ [y])),ys)

-- i.e., [1,2,3] becomes 123. The reversal could be optimized away.
listToInt :: [Int] -> Integer
listToInt ds = sum [toInteger (10^k * d) | (k,d) <- zip [0..] (reverse ds)]

main :: IO ()
main = do
  banks <- parseInput "input.txt"
  putStrLn $
    "Part 1: " ++
    show (sum . map (listToInt . (maxjoltage 2)) $ banks)
  putStrLn $
    "Part 2: " ++
    show (sum . map (listToInt . (maxjoltage 12)) $ banks)
