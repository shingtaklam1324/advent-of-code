module Day2 (day2part1, day2part2) where

import Data.List.Split
import Data.List.Utils

-- checkPw :: String -> Bool
checkPwPart1 s =
    let [num,[char, colon],pw] = words s 
        [n1, n2]               = map read $ splitOn "-" num :: [Int]
        c                      = countElem char pw in
    n1 <= c && c <= n2

day2part1 :: IO ()
day2part1 = do
    s <- readFile "inputs/day2.txt"
    let l = map checkPwPart1 $ lines s
    print $ countElem True l

checkPwPart2 s =
    let [num,[char, colon],pw] = words s 
        [n1, n2]               = map read $ splitOn "-" num :: [Int]
        a                      = pw !! (n1-1)
        b                      = pw !! (n2-1)
    in (a == char || b == char) && not (a == char && b == char)

day2part2 :: IO ()
day2part2 = do
    s <- readFile "inputs/day2.txt"
    let l = map checkPwPart2 $ lines s
    print $ countElem True l
