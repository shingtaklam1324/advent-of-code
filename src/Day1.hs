module Day1 (day1part1, day1part2) where

findPairSumTo :: Int -> [Int] -> [Int]
findPairSumTo n l = filter (\x -> n - x `elem` l) l

day1part1 :: IO ()
day1part1 = do
    s <- readFile "inputs/day1.txt"
    let l = map read $ words s :: [Int]
    let res = findPairSumTo 2020 l
    if length res == 2 then
        print $ product res
    else
        putStrLn "Error"

findThreeSumTo :: Int -> [Int] -> [[Int]]
findThreeSumTo n l = filter (\s -> length s == 2) $ map (\x -> findPairSumTo (n - x) l) l

day1part2 :: IO ()
day1part2 = do
    s <- readFile "inputs/day1.txt"
    let l = map read $ words s :: [Int]
    let res = head $ findThreeSumTo 2020 l
    print $ (2020 - sum res) * product res
