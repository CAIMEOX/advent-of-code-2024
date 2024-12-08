import Data.Map (Map, delete, elems, fromListWith, insertWith)
import Data.Set (Set, fromList, unions)

type Loc = (Int, Int)

main = do
  (m, bound) <- parseInput <$> readFile "input/day8.txt"
  let sets = (<$> m) . flip antinodes bound <$> [part1, part2]
  mapM_ (print . length . unions . elems) sets

parseInput :: String -> (Map Char [Loc], Loc)
parseInput xs = (m, bound)
  where
    ys = lines xs
    bound@(x0, y0) = (length (head ys) - 1, length ys - 1)
    ranges = [(x, y) | x <- [0 .. x0], y <- [0 .. y0]]
    m = delete '.' $ fromListWith (++) (zip (concat ys) (pure <$> ranges))

part1, part2 :: Loc -> Loc -> [Loc]
part1 (y0, x0) (y1, x1) = [(2 * y1 - y0, 2 * x1 - x0)]
part2 (y0, x0) (y1, x1) = zip [y1, 2 * y1 - y0 ..] [x1, 2 * x1 - x0 ..]

antinodes f (x0, y0) pos = fromList [z | p1 <- pos, p2 <- pos, p1 /= p2, z <- takeWhile inbound $ f p1 p2]
  where
    inbound (x, y) = 0 <= x && x <= x0 && 0 <= y && y <= y0