import Data.Array.Unboxed (Array, bounds, inRange, listArray, (!))
import Data.Set (Set, empty, singleton, unions)

main :: IO ()
main = do
  file <- readFile "input/day10.txt"
  let (zeros, grid) = parseInput file
  print $ sum $ map (length . compute grid (const empty, singleton, unions)) zeros
  print $ sum $ map (compute grid (const 0, const 1, sum)) zeros

parseInput :: [Char] -> ([(Int, Int)], Array (Int, Int) Int)
parseInput str = (zeros, grid)
  where
    xs = lines str
    (w, h) = (length (head xs), length xs)
    grid = (\x -> read [x]) <$> listArray ((0, 0), (w - 1, h - 1)) (concat xs)
    zeros = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1], grid ! (x, y) == 0]

compute arr (zero, one, concat) (x, y) = go (x, y) 0
  where
    check (x, y) = inRange (bounds arr) (x, y)
    go o _ | not $ check o = zero (x, y)
    go (x, y) 9 = one (x, y)
    go (x, y) n = concat [go np (n + 1) | (x', y') <- [(0, -1), (0, 1), (1, 0), (-1, 0)], let np = (x + x', y + y'), check np, arr ! np == n + 1]
