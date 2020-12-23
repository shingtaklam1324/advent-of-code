module Day6 (day6part1, day6part2) where

import Data.List.Split
import Data.List.Utils

{-
For Part 1, all we need to do is to concatenate the inputs of all of the lines in each entry, and then
deduplicate.
-}
day6part1 :: IO ()
day6part1 = do
    s <- readFile "inputs/day6.txt"
    let l = sum $ map (length . uniq . filter (/= '\n')) $ splitOn "\n\n" s
    print l

{-
For Part 2, we can just check all characters in `a` to `z` to check if it is in every line in the
entry.
-}
day6part2 :: IO ()
day6part2 = do
    s <- readFile "inputs/day6.txt"
    let l = sum $ map (length . (\ s -> [x | x <- ['a' .. 'z'], all (x `elem`) s]) . lines) $ splitOn "\n\n" s
    print l
