import Data.List (findIndex)
import Data.Map (Map, assocs, fromList, fromListWith)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  stones <- map read . words <$> readFile "input/day11.txt"
  print $ process 25 stones
  print $ process 75 stones

digits :: (Ord a, Num a) => a -> Int
digits z = fromJust (findIndex (> z) lst)
  where lst = iterate (* 10) 1

process :: Int -> [Int] -> Int
process n input = sum (times n blinks (fromList [(i, 1) | i <- input]))
  where
    times 0 _ x = x
    times n f x = times (n - 1) f (f x)

blinks :: Map Int Int -> Map Int Int
blinks stones = fromListWith (+) [(stone', n) | (stone, n) <- assocs stones, stone' <- blink stone]

blink :: Int -> [Int]
blink 0 = [1]
blink n
  | let len = digits n,
    even len, (l, r) <- n `divMod` (10 ^ (len `div` 2)) = [l, r]
  | otherwise = [n * 2024]