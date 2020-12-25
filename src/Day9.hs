module Day9 where

import Data.List.Utils

{-
Check if a number is the sum of two elements in a list. We do this by for each
`x` in the list whether `n - x` is in the list, and that we have not chosen the
same number twice.
-}
isSumPair :: (Eq a, Num a) => a -> [a] -> Bool
isSumPair n l = any (\x -> n - x `elem` l && not (2 * x == n && countElem x l == 1)) l

{-
To find the first number which is not the sum of the previous 25
-}
findFirstNotSum :: [Int] -> Int
findFirstNotSum l = aux (take 25 l) (drop 25 l)
  where
    aux :: [Int] -> [Int] -> Int
    aux acc [] = -9999
    aux acc (h : t) =
        if isSumPair h acc then
            aux (drop 1 acc ++ [h]) t
        else
            h

day9part1 :: IO ()
day9part1 = do
    s <- readFile "inputs/day9.txt"
    let l = map read $ lines s
    let res = findFirstNotSum l
    print res

findNSumTo :: Int -> [Int] -> [Int]
findNSumTo n (h : t) =
    let s = sum (h : t)
        l' = takeWhileList (\l -> s - sum l < n) (h : t) in 
    if sum l' == n then
        l'
    else
        findNSumTo n t

day9part2 :: IO ()
day9part2 = do
    s <- readFile "inputs/day9.txt"
    let l = map read $ lines s
    let res = findFirstNotSum l
    let range = findNSumTo res l
    print $ minimum range + maximum range 
