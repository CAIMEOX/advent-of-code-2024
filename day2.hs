import Data.List

main :: IO ()
main = do
  file <- readFile "input/day2.txt"
  print $ process safe file
  print $ process safe' file

process part x = length $ filter part (map read . words <$> lines x)

safe :: [Int] -> Bool
safe x = (all (uncurry (<)) tx || all (uncurry (>)) tx) && elemLeq3
  where
    tx = zip x (tail x)
    elemLeq3 = all (\(a, b) -> abs (b - a) <= 3) tx

safe' :: [Int] -> Bool
safe' = any safe . allSubs

allSubs :: [a] -> [[a]]
allSubs xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]
