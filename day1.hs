import Data.Bifunctor (bimap)
import Data.List (sort)

main :: IO ()
main = do
  file <- readFile "input/day1.txt"
  print $ process file
  print $ process' file

pairs :: String -> (Int, Int)
pairs x = let [a, b] = words x in (read a, read b)

process' :: String -> Int
process' x = sum $ map ((*) <*> (`count` s2)) s1
  where
    (s1, s2) = unzip $ map pairs $ lines x

process :: String -> Int
process x = sum $ zipWith ((abs .) . subtract) s1 s2
  where
    (s1, s2) = bimap sort sort $ unzip $ map pairs $ lines x

count :: Int -> [Int] -> Int
count x = length . filter (== x)