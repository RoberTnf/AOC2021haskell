{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import Data.ByteString (count)
import Data.Either (lefts, rights)
import Data.List (transpose)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

type BingoCell = Maybe Int

type BingoBoard = [[BingoCell]]

type DrawnNumbers = [Int]

main :: IO ()
main = do
  rawContent <- TIO.readFile "input.txt"
  -- print . solveFirst $ rawContent
  print . solveSecond $ rawContent

solveFirst input =
  let (drawnNumbers, bingoBoards) = processInput input
   in drawUntilWinner drawnNumbers bingoBoards

drawUntilWinner :: [Int] -> [BingoBoard] -> Int
drawUntilWinner drawNumbers = drawUntilWinner' drawNumbers (-1)

drawUntilWinner' (n : ns) lastN bingoBoards =
  let winner = filter isSolved bingoBoards
   in if not (null winner) then lastN * sumBoard (head winner) else drawUntilWinner' ns n $ map (draw n) bingoBoards
drawUntilWinner' _ _ _ = error "No winner"

sumBoard :: BingoBoard -> Int
sumBoard =
  foldr
    ( flip
        ( foldr
            ( \el c -> case el of
                Just x -> x + c
                Nothing -> c
            )
        )
    )
    0

draw :: Int -> BingoBoard -> BingoBoard
draw n = map (map (\el -> if Just n == el then Nothing else el))

processInput :: T.Text -> (DrawnNumbers, [BingoBoard])
processInput input =
  let linedInput = T.lines input
      drawnNumbers = map fst . rights . map TR.decimal . T.splitOn "," $ head linedInput
      bingoBoard = map (map (map fst . rights . map TR.decimal . T.words)) . splitOn "" . tail $ T.lines input :: [[[Int]]]
      bingoBoard' = map (map (map Just)) bingoBoard
   in (drawnNumbers, bingoBoard')

splitOn :: T.Text -> [T.Text] -> [[T.Text]]
splitOn c l = case dropWhile (c ==) l of
  [] -> []
  s -> el : splitOn c s'
    where
      (el, s') = break (c ==) s

-- |
-- >>> isSolved [[Just 3, Just 2], [Nothing, Nothing]]
-- True
-- >>> isSolved [[Just 3, Nothing], [Just 0, Nothing]]
-- True
-- >>> isSolved [[Just 3, Nothing], [Just 0, Just 2]]
-- False
isSolved board =
  let isSolvedRowWise = any (all isNothing) board
      isSolvedColWise = any (all isNothing) $ transpose board
      solvedBingoBoard = [[Just 3, Just 2], [Nothing, Nothing]]
   in isSolvedRowWise || isSolvedColWise

-------------------------- Second

solveSecond input =
  let (drawnNumbers, bingoBoards) = processInput input
   in drawUntilLoser drawnNumbers bingoBoards

--drawUntilLoser :: [Int] -> [BingoBoard] -> Int
drawUntilLoser drawNumbers = drawUntilLoser' drawNumbers (-1)

drawUntilLoser' (n : ns) lastN bingoBoards =
  let losers = filter (not . isSolved) bingoBoards
      biggestLoser = head bingoBoards
   in --in if length losers == 1 then lastN * sumBoard (head losers) else drawUntilLoser' ns n $ map (draw n) bingoBoards
      if null losers then lastN * sumBoard biggestLoser else drawUntilLoser' ns n $ map (draw n) losers
drawUntilLoser' _ _ _ = error "No loser"