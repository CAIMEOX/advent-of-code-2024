import Control.Monad (guard)
import Data.List (unfoldr)
import Data.Map.Lazy qualified as Map
import Data.Set qualified as Set

main = do
  m <- parseInput <$> readFile "input/day12.txt"
  mapM_ (\f -> print . sum $ (\x -> f x * length x) <$> distinct m) [perimeter, corners]

parseInput :: [Char] -> Map.Map (Int, Int) Char
parseInput xs = Map.fromList [((x, y), ys !! x !! y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
  where
    ys = lines xs
    (w, h) = (length $ head ys, length ys)

dfs :: (Ord t) => (t -> [t]) -> [t] -> [t]
dfs next = loop Set.empty
  where
    loop present xs = case xs of
      [] -> []
      x : xs
        | Set.member x present -> loop present xs
        | otherwise -> x : loop (Set.insert x present) (next x <> xs)

distinct :: Map.Map (Int, Int) Char -> [Set.Set (Int, Int)]
distinct = unfoldr f
  where
    f input = do
      (start, key) <- Map.lookupMin input
      let components = Set.fromList $ dfs (next input key) [start]
      pure (components, Map.withoutKeys input components)
    next m k p@(x, y) = [p' | p' <- neighbors p, Map.lookup p' m == Just k]

neighbors :: (Num a, Num b) => (a, b) -> [(a, b)]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]

perimeter :: Set.Set (Int, Int) -> Int
perimeter region = sum [1 | x <- Set.toList region, y <- neighbors x, y `Set.notMember` region]

corners :: (Num b, Num a, Ord a, Ord b) => Set.Set (a, b) -> Int
corners xs =
  sum $ map (length . flip Set.filter xs)
      [ \(x, y) -> not (f (x, y - 1)) && (not (f (x + 1, y)) || f (x + 1, y - 1)),
        \(x, y) -> not (f (x, y + 1)) && (not (f (x + 1, y)) || f (x + 1, y + 1)),
        \(x, y) -> not (f (x + 1, y)) && (not (f (x, y - 1)) || f (x + 1, y - 1)),
        \(x, y) -> not (f (x - 1, y)) && (not (f (x, y + 1)) || f (x - 1, y + 1))
      ]
  where f = flip Set.member xs