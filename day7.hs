import Control.Arrow
import Control.Monad (foldM)
import Data.Foldable (find)
import Data.Maybe (fromJust)

type Ops = [Int -> Int -> Int]

main = do
  file <- parseInput <$> readFile "input/day7.txt"
  mapM_ (print . sum . map fst . flip filter file . compute) [[(+), (*)], [(+), (*), concatenate]]
  where
    concatenate x y = x * mag y + y
    mag z = fromJust $ find (> z) $ iterate (* 10) 10

parseInput :: String -> [(Int, [Int])]
parseInput = lines >>> map (break (== ':') >>> (read *** map read . words . tail))

compute :: Ops -> (Int, [Int]) -> Bool
compute ops (x, y : ys) = x `elem` foldM apply y ys
  where
    apply a y = map (\op -> a `op` y) ops
