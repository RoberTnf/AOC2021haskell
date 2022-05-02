-- First time doing haskell in a while.
-- lsp catched some simplifications, looks nice!

import System.IO ()

main :: IO ()
main = do
  rawContent <- readFile "input.txt"
  let input = map read . lines $ rawContent
  print . solveFirst $ input
  print . solveSecond $ input

solveFirst :: [Int] -> Int
solveFirst x = length . filter (uncurry (<)) . zip x $ tail x

-- Took me some time to figure out sum doesn't work over tuples (not Foldable).
solveSecond :: [Int] -> Int
solveSecond x =
  let firstTail = tail x
      secondTail = tail firstTail
      trios = zip3 x firstTail secondTail
      sums = map (\(x, y, z) -> x + y + z) trios
   in length $ filter (uncurry (<)) . zip sums $ tail sums