import Text.Parsec

data Opcode = Do | Dont | Mul Int Int | Nop deriving (Show, Eq)

isMul (Mul _ _) = True
isMul _ = False

parseInput xs = case parse (many1 (try parseLine <|> try parseDo <|> try parseDont <|> parseNop)) "" xs of
  Left err -> error $ show err
  Right x -> x

parseLine :: Parsec String a Opcode
parseLine = do
  b <- string "mul(" *> (read <$> many1 digit) <* char ','
  c <- read <$> many1 digit <* char ')'
  return (Mul b c)

parseDo = string "do()" >> return Do

parseDont = string "don't()" >> return Dont

parseNop = anyChar >> return Nop

runVM :: [Opcode] -> Int
runVM = loop True
  where
    loop _ [] = 0
    loop _ (Do : xs) = loop True xs
    loop _ (Dont : xs) = loop False xs
    loop False (Mul a b : xs) = loop False xs
    loop True (Mul a b : xs) = a * b + loop True xs

main :: IO ()
main = do
  file <- readFile "input/day3.txt"
  let cmds = filter (Nop /=) $ parseInput file
  print $ runVM (filter isMul cmds)
  print $ runVM cmds