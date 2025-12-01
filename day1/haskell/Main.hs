type DialPos = Int
type Rotation = Int

-- Constants
_dialSize = 100
_dialStart = 50

-- Returns a list of signed rotations
parseInput :: FilePath -> IO [Rotation]
parseInput file = do
  input <- readFile file
  return $ parser input
  where
    parser = map parse . lines
    parse (c:cs) = case c of
      'L' -> (-1) * read cs
      'R' -> read cs
    parse _ = undefined

-- Perform one rotation
rotate :: DialPos -> Rotation -> DialPos
rotate dialPos rot = (dialPos + rot) `mod` _dialSize

-- Perform one rotation, but keep track of how many times we've passed 0
rotate2 :: DialPos -> Rotation -> (DialPos, Int)
rotate2 dialPos rot =
  let (fullRot, fracRot) = rot `quotRem` _dialSize
      newDialPos = rotate dialPos fracRot
      passedZero | (dialPos == 0) = False
                 | (dialPos + fracRot <= 0 || dialPos + fracRot >= _dialSize) = True
                 | otherwise = False
  in
      (newDialPos, abs fullRot + fromEnum passedZero)

main :: IO ()
main = do
  rotations <- parseInput "input.txt"
  let part1 = length .
              filter (== 0) .
              scanl rotate _dialStart $ rotations

  let part2 = sum .
              map (\x -> snd x) .
              scanl (\(d,r) -> rotate2 d) (_dialStart, 0) $ rotations
              --We want to scan as in part1, but our function returns a tuple. We don't care about
              --the second argument of the tuple when we perform rotations, we just want to keep
              --track of it for later.

  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
