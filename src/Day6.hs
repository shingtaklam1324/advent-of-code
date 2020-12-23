module Day6 (day6part1, day6part2) where

import Data.List.Split
import Data.List.Utils

day6part1 :: IO ()
day6part1 = do
    s <- readFile "inputs/day6.txt"
    let l = sum $ map (length . uniq . filter (/= '\n')) $ splitOn "\n\n" s
    print l

day6part2 :: IO ()
day6part2 = do
    s <- readFile "inputs/day6.txt"
    let l = sum $ map (length . (\ s -> [x | x <- ['a' .. 'z'], all (x `elem`) s]) . lines) $ splitOn "\n\n" s
    print l
