module Day1 (day1part1, day1part2) where

{-
To find a pair of numbers in `l` that sum to `n`, suffices to find `x` such that
`x` and `n-x` are both in `l`. Using a `filter` and checking whether a number is
an element of `l` is a simple solution, albeit O(n^2).

Also, if `x` is in the remaining list, so is `n-x`. In this case we want both,
and also there is only two, but if we only wanted one, or if there was multiple
pairs, then this may not be suitable.
-}
findPairSumTo :: Int -> [Int] -> [Int]
findPairSumTo n l = filter (\x -> n - x `elem` l) l

{-
To do the first part of day 1, we use the function defined above to find the two
numbers which add to 2020. Then we multiply them together.
-}
day1part1 :: IO ()
day1part1 = do
    s <- readFile "inputs/day1.txt"
    let l = map read $ words s :: [Int]
    let res = findPairSumTo 2020 l
    if length res == 2 then
        print $ product res
    else
        putStrLn "Error"

{-
To find three numbers in `l` which sum to `n`, for each such `x` we have two
numbers in `l` which sum to `n-x`. Note that this is not perfect, as if 
`a+a+b=n`, then this function will say that [a,a,b] is a solution, even if `a`
only appears in `l` once. It wasn't an issue with my specific inputs, however
it may be an issue for other inputs.
-}
findThreeSumTo :: Int -> [Int] -> [[Int]]
findThreeSumTo n l = filter (\s -> length s == 2) $ map (\x -> findPairSumTo (n - x) l) l

day1part2 :: IO ()
day1part2 = do
    s <- readFile "inputs/day1.txt"
    let l = map read $ words s :: [Int]
    let res = head $ findThreeSumTo 2020 l
    print $ (2020 - sum res) * product res
