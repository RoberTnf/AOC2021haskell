-- Cool problem, involving some simple pattern matching.
-- In the end, it took some struggling with a fold, but solution is quite simple

main :: IO ()
main = do
  rawContent <- readFile "input.txt"
  let input = lines rawContent
  print . solveFirst $ input
  print . solveSecond $ input

solveFirst :: (Read a, Num a) => [String] -> a
solveFirst x =
  let instructions = map ((\[instruction, len] -> (instruction, read len)) . words) x
      (h, d) = foldl (\(h, d) (instruction, len) -> process instruction len h d) (0, 0) instructions
   in h * d

process :: Num a => [Char] -> a -> a -> a -> (a, a)
process "forward" len h d = (h + len, d)
process "up" len h d = (h, d - len)
process "down" len h d = (h, d + len)
process _ _ _ _ = error "Unexpected instruction"

solveSecond :: (Read a, Num a) => [String] -> a
solveSecond x =
  let instructions = map ((\[instruction, len] -> (instruction, read len)) . words) x
      (h, d, aim) = foldl (\(h, d, aim) (instruction, len) -> processSecond instruction len h d aim) (0, 0, 0) instructions
   in h * d

processSecond :: Num a => [Char] -> a -> a -> a -> a -> (a, a, a)
processSecond "forward" len h d aim = (h + len, d + aim * len, aim)
processSecond "up" len h d aim = (h, d, aim - len)
processSecond "down" len h d aim = (h, d, aim + len)
processSecond _ _ _ _ _ = error "Unexpected instruction"