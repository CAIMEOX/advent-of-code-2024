import Data.List (tails, transpose)

newtype Matrix a = Matrix [[a]] deriving (Show)

get3x3 :: [[a]] -> Int -> Int -> [[a]]
get3x3 xs y x = map (take 3 . drop x) (take 3 $ drop y xs)

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

diagonals :: [[a]] -> [[a]]
diagonals =
  (++)
    <$> rotate . zipWith drop [0 ..]
    <*> transpose . zipWith drop [1 ..] . transpose

windows :: Int -> [a] -> [[a]]
windows n xs = zipWith (const id) (drop (n - 1) xs) (map (take n) (tails xs))

validMat :: [[Char]] -> Int
validMat xs = sum (map f xs) + sum (map f (transpose xs)) + g xs + g (rotate xs)
  where
    f ys = length $ filter valid (windows 4 ys)
    g zs = sum [1 | x <- concatMap (windows 4) (diagonals zs), valid x]
    valid x = x == "XMAS" || x == "SAMX"

xmas :: [[Char]] -> Bool
xmas xs = all valid [diagonals xs !! 2, diagonals (rotate xs) !! 2]
  where
    valid x = x == "MAS" || x == "SAM"

main = do
  file <- readFile "input/day4.txt"
  let m = lines file
  let (h, w) = (length m - 3, length (head m) - 3)
  let mats3x3 = [1 | y <- [0 .. w], x <- [0 .. h], xmas (get3x3 m y x)]
  print $ validMat m
  print $ sum mats3x3