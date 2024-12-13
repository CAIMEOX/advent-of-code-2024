{-# LANGUAGE MonadComprehensions #-}
import Data.Maybe (mapMaybe)
import Text.Parsec

type Loc = (Integer, Integer)

parseInput :: [String] -> (Loc, Loc, Loc)
parseInput [buttonA, buttonB, prize] = (bA, bB, p)
  where
    unwrap (Right x) = x
    bA = parseLoc "Button A: X+" ", Y+" buttonA
    bB = parseLoc "Button B: X+" ", Y+" buttonB
    p = parseLoc "Prize: X=" ", Y=" prize

parseLoc s1 s2 = unwrap . parse go ""
  where
    unwrap (Right x) = x
    go = [(x, y) | _ <- string s1, x <- read <$> many1 digit, _ <- string s2, y <- read <$> many1 digit]

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

solve :: (Loc, Loc, Loc) -> Maybe Loc
solve ((a, b), (a', b'), (x', y'))
  | det == 0 || sX * a + sY * a' /= x' || sX * b + sY * b' /= y' = Nothing
  | otherwise = Just (sX, sY)
  where
    det = a * b' - a' * b
    [sX, sY] = (`div` det) <$> [x' * b' - a' * y', a * y' - x' * b]

process :: [(Loc, Loc, Loc)] -> Integer
process ls = sum $ map (\(x, y) -> 3 * x + y) $ mapMaybe solve ls

main = do
  m1 <- map parseInput . group 3 . filter (/= "") . lines <$> readFile "input/day13.txt"
  print $ process m1
  print $ process ((\(a, b, (x, y)) -> (a, b, (x + 10000000000000, y + 10000000000000))) <$> m1)
