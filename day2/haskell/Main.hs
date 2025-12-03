{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Integer.Logarithms ( integerLogBase# )
import GHC.Exts (Int(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List

numDigits :: Integer -> Integer
numDigits x = (+1) . toInteger $ I# (integerLogBase# 10 x)

-- duplicate m n returns nn...n (m times), where nn...n means concatenate the integers
duplicate :: Integer -> Integer -> Integer
duplicate m n = magicNumber * n
  where
    nd = numDigits n
    magicNumber = sum [10^(nd * k) | k <- [0..(m-1)]]

-- minimum n such that nn...n (m times) >= x
-- don't try to understand this
minIID :: Integer -> Integer -> Integer
minIID m x
  | (nd `mod` m /= 0) = 10^(nd `div` m)
  | otherwise = if (duplicate (m-1) front >= back) then front else (front+1)
  where
    nd = numDigits x
    (front,back) = x `divMod` (10^((m-1) * (nd `div` m)))

-- returns all invalid IDs, where d is the number of duplications in each ID.
-- for example, for 121212, d is 3 and for 11231123, d is 2.
-- lazy evaluation does this pretty efficiently
invalids :: Integer -> (Integer,Integer) -> [Integer]
invalids d (start,end) = takeWhile (<= end)
                          . map (duplicate d)
                          $ [minIID d start..]

-- returns all invalid IDs, in the sense of part 2.
-- it does this by just enumerating all invalid IDs of each 'duplication type', using the function above.
allInvalids :: (Integer,Integer) -> [Integer]
allInvalids (start,end) = nub $
                            [2..numDigits end] >>= (\m -> invalids m (start,end))

parseInput :: FilePath -> IO [(Integer,Integer)]
parseInput file = do
  text <- TIO.readFile file
  return $
    map (\[x,y] -> (readT x, readT y))
     . map (T.splitOn "-")
     . (T.splitOn ",")
     $ text
  where
    readT = read . T.unpack

main :: IO ()
main = do
  input <- parseInput "input.txt"
  putStrLn $
    "Part 1: " ++
    show (sum . concatMap (invalids 2) $ input)
  putStrLn $
    "Part 2: " ++
    show (sum . concatMap allInvalids $ input)
