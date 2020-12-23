module Day5 (day5part1, day5part2) where

import Data.Char (digitToInt)
import Data.List (foldl', sort)

-- A function to convert a binary string to an integer
binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

{-
The key point to notice in this task is that the actual seat doesn't matter,
and by changing the letters to `1` and `0`, we get a binary number, which is
precisely the ID of the seat.

Therefore, we can just convert all of the seats to integers.
-}

boardingPassMap :: Char -> Char
boardingPassMap 'F' = '0'
boardingPassMap 'B' = '1'
boardingPassMap 'L' = '0'
boardingPassMap 'R' = '1'

{-
In this case, the maximum integer is the maximum ID
-}
day5part1 :: IO ()
day5part1 = do
    s <- readFile "inputs/day5.txt"
    let l = maximum $ map (binToInt . map boardingPassMap) $ lines s
    print l

{-
For Part 2, we can use Haskell's list comprehension to find `x` such that
`x - 1` and `x + 1` are both in `l`, but `x` itself is not.
-}
day5part2 :: IO ()
day5part2 = do
    s <- readFile "inputs/day5.txt"
    let l = sort $ map (binToInt . map boardingPassMap) $ lines s
    let y = [x | x <- [1..1000],
                 x - 1 `elem` l,
                 x `notElem` l,
                 x + 1 `elem` l]
    print $ head y
