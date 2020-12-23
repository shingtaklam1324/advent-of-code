module Day5 (day5part1, day5part2) where

import Data.Char (digitToInt)
import Data.List (foldl', sort)

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

boardingPassMap :: Char -> Char
boardingPassMap 'F' = '0'
boardingPassMap 'B' = '1'
boardingPassMap 'L' = '0'
boardingPassMap 'R' = '1'

-- parseBoardingPass :: String -> (Int, Int)
-- parseBoardingPass p =
--     let p' = map boardingPassMap p
--         rowStr = take 7 p'
--         colStr = drop 7 p' in
--     (binToDec rowStr, binToDec colStr)

day5part1 :: IO ()
day5part1 = do
    s <- readFile "inputs/day5.txt"
    let l = maximum $ map (binToInt . map boardingPassMap) $ lines s
    print l

day5part2 :: IO ()
day5part2 = do
    s <- readFile "inputs/day5.txt"
    let l = sort $ map (binToInt . map boardingPassMap) $ lines s
    let y = [x | x <- [1..1000],
                 x - 1 `elem` l,
                 x `notElem` l,
                 x + 1 `elem` l]
    print $ head y
