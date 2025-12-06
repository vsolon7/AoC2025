{-# LANGUAGE OverloadedStrings #-}
import Data.List (transpose, foldl1')
import Data.Char (isSpace)
import Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Applicative
import Data.Functor

-- Attoparsec is overkill for this, but it's nice to get some practice in.

-- p $> x means "run p; if p is successful, discard its input and return x."
parseMult :: Parser (Int -> Int -> Int)
parseMult = char '*' $> (*)

parseAdd :: Parser (Int -> Int -> Int)
parseAdd = char '+' $> (+)

-- For part 1
parseNumLines :: Parser [[Int]]
parseNumLines = parseNumLine `sepBy` endOfLine
  where
    parseNumLine = do
      skipWhile (== ' ') -- isSpace matches on \n as well, so we use (== ' ')
      many1 (decimal <* skipWhile (== ' ')) -- also skips over trailing whitespace, unlike sepBy

-- For part 1
parseOpLine :: Parser [Int -> Int -> Int]
parseOpLine = do
  skipWhile isSpace
  (parseAdd <|> parseMult) `sepBy` skipSpace

parseInput1 :: Parser ([[Int]],[Int -> Int -> Int])
parseInput1 = do
  numss <- parseNumLines
  ops <- parseOpLine
  return (transpose numss,ops)

-- Surprisingly easier than part 1
parseInput2 :: Parser [(Int -> Int -> Int, [Int])]
parseInput2 = many parseBlock
  where
    parseBlock = do
      skipSpace
      ns <- many (decimal <* skipSpace)
      op <- parseMult <|> parseAdd
      return (op, ns)

one :: FilePath -> IO ()
one file = do
  text <- TIO.readFile file
  case parseOnly parseInput1 text of
    Left rem -> do
      putStrLn ("Parse Failed. Remaining string: " ++ rem)
    Right (nss, ops) -> do
      let ans = sum (zipWith foldl1' ops nss)
      putStrLn ("Part 1: " ++ show ans)

two :: FilePath -> IO ()
two file = do
  text <- readFile file
  let torturedText = T.pack . concat . reverse . transpose . lines $ text
  case parseOnly parseInput2 torturedText of
    Left rem -> do
      putStrLn ("Parse Failed. Remaining string: " ++ rem)
    Right problems -> do
      let ans = sum . map (uncurry foldl1') $ problems
      putStrLn ("Part 2: " ++ show ans)

main :: IO ()
main = do
  one "input.txt"
  two "input.txt"
