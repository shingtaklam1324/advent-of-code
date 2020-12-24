module Day10 where

import Data.List
import Data.List.Utils

day10part1 :: IO ()
day10part1 = do
    s <- readFile "inputs/day10.txt"
    let l = sort $ map read $ lines s :: [Int]
    let l' = map (\x -> -x) $ zipWith (-) (0 : l) (l ++ [last l + 3])
    let a = countElem 1 l'
    let b = countElem 3 l'
    print $ a * b
