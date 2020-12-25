module Day10 where

import Data.List
import Data.List.Utils
import qualified Data.Map.Strict as Map

day10part1 :: IO ()
day10part1 = do
    s <- readFile "inputs/day10.txt"
    let l = sort $ map read $ lines s :: [Int]
    let l' = map (\x -> -x) $ zipWith (-) (0 : l) (l ++ [last l + 3])
    let a = countElem 1 l'
    let b = countElem 3 l'
    print $ a * b

step :: [Int] -> Map.Map Int Int -> Map.Map Int Int
step [] m = m
step (h : t) m = 
    let m' = step t m in
    Map.insert h (Map.findWithDefault 0 (h + 1) m' + Map.findWithDefault 0 (h + 2) m' + Map.findWithDefault 0 (h + 3) m') m'

day10part2 :: IO ()
day10part2 = do
    s <- readFile "inputs/day10.txt"
    let l = sort $ map read $ lines s :: [Int]
    let m = Map.singleton (last l + 3) 1
    let res = step (0 : l) m
    print $ Map.lookup 0 res
