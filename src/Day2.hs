module Day2 (day2part1, day2part2) where

import Data.List.Split
import Data.List.Utils

{-
For this, we first parse the input data, getting the numbers and character that
we want out of each line. Then we can just count how many times the specific
character appears in the password, and return whether it is in the range or not.
-}
checkPwPart1 :: String -> Bool
checkPwPart1 s =
    let [num,[char, colon],pw] = words s 
        [n1, n2]               = map read $ splitOn "-" num :: [Int]
        c                      = countElem char pw in
    n1 <= c && c <= n2

{-
The first part of day 2 just requires us to check each line of the input data to
determine the number of valid inputs.
-}
day2part1 :: IO ()
day2part1 = do
    s <- readFile "inputs/day2.txt"
    let l = length $ filter checkPwPart1 $ lines s
    print l

{-
For the second part, we want to check the two specific indices of the password,
and then all that is required is to check that exactly one matches the
character required.
-}
checkPwPart2 :: String -> Bool
checkPwPart2 s =
    let [num,[char, colon],pw] = words s 
        [n1, n2]               = map read $ splitOn "-" num :: [Int]
        a                      = pw !! (n1-1)
        b                      = pw !! (n2-1)
    in (a == char || b == char) && not (a == char && b == char)

{- 
The IO part of part 2 is the same as part 1
 -}
day2part2 :: IO ()
day2part2 = do
    s <- readFile "inputs/day2.txt"
    let l = length $ filter checkPwPart2 $ lines s
    print l
