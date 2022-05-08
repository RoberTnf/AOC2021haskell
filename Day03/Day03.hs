import Data.Char (digitToInt)

main :: IO ()
main = do
  rawContent <- readFile "input.txt"
  let input = lines rawContent
  --print . solveFirst $ input
  print . solveSecond $ input

solveFirst x =
  let columnCounter = take nColumns [0, 0 ..] :: [Int]
      nColumns = length $ head x
      rowAdder cc row =
        let zipped = zip cc row
         in map (uncurry adder) zipped
      columnsCounted = foldr rowAdder columnCounter x
      gammaRate = toDecimal . map discriminateGamma $ columnsCounted
      epsilonRate = toDecimal . map discriminateEpsilon $ columnsCounted
   in gammaRate * epsilonRate

toDecimal x = sum . zipWith (\i c -> c * 2 ^ i) [0 ..] $ reverse x

adder '0' c = c - 1
adder '1' c = c + 1
adder c _ = error $ "Bad input, expected binary char got " ++ [c]

discriminateGamma x
  | x > 0 = 1
  | otherwise = 0

discriminateEpsilon x
  | x < 0 = 1
  | otherwise = 0

-------------------- Second

solveSecond x =
  let o2 = toDecimal . map digitToInt . head $ recursive x 0 True
      co2 = toDecimal . map digitToInt . head $ recursive x 0 False
   in co2 * o2

recursive (x1 : x2 : xs) i isO2 = recursive (filterByColumn (x1 : x2 : xs) i isO2) (i + 1) isO2
recursive x _ _ = x

-- Way too complicated. Should be simplified to share code. Too tired.
filterByColumn input columnID isO2
  | isO2 =
    let discriminator = case mostCommon of
          '2' -> '1'
          x -> x
        filteredIdx = map fst . filter ((discriminator ==) . snd) $ zip [0 ..] column
     in map snd . filter (\(i, r) -> i `elem` filteredIdx) $ zip [0 ..] input
  | otherwise =
    let discriminator = case mostCommon of
          '2' -> '0'
          '1' -> '0'
          '0' -> '1'
          x -> error $ "Unexpected input " ++ show x
        filteredIdx = map fst . filter ((discriminator ==) . snd) $ zip [0 ..] column
     in map snd . filter (\(i, r) -> i `elem` filteredIdx) $ zip [0 ..] input
  where
    mostCommon = mostCommonInColumn column
    -- there has to be a better way to get the column
    column = map (head . map snd . filter (\(i, _) -> i == columnID) . zip [0 ..]) input

mostCommonInColumn column
  | counter > 0 = '1'
  | counter == 0 = '2'
  | otherwise = '0'
  where
    counter = foldr adder 0 column