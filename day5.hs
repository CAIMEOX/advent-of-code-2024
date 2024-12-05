import Data.Bifunctor (Bifunctor (bimap))
import Data.List (nub, sortBy, (\\))

type Rel = [(Int, Int)]

cmpRel :: Rel -> Int -> Int -> Ordering
cmpRel rel x y = if (x, y) `elem` rel then LT else GT

o2b LT = True
o2b GT = False

readInput :: String -> (Rel, [[Int]])
readInput xs = (map parseRule p, map parseData rs)
  where
    (p, _ : rs) = span (/= "") (lines xs)
    parseRule x = bimap read (read . tail) (span (/= '|') x)
    parseData = map read . wordsWhen (== ',')

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> let (w, s'') = break p s' in w : wordsWhen p s''

isSorted :: (b -> b -> Ordering) -> [b] -> Bool
isSorted f xs = (all (o2b . uncurry f) . zip xs) $ tail xs

main :: IO ()
main = do
  file <- readFile "input/day5.txt"
  let (r, d) = readInput file
  let cmp = cmpRel r
  let (oks, noks) = (filter (isSorted cmp) d, d \\ oks)
  print $ sum $ map mid oks
  print $ sum $ map (mid . sortBy cmp) noks
  where
    mid xs = xs !! (length xs `div` 2)
