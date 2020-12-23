module Day3 (day3part1, day3part2) where

-- change in row, change in col, data, num trees
countTrees :: Int -> Int -> [[Char]] -> Int
countTrees p q lst = 
    let w = length $ head lst
        l = length lst in
    countTreesAux p q w l lst
  where
    countTreesAux :: Int -> Int -> Int -> Int -> [[Char]] -> Int
    countTreesAux r c w l lst = 
        if r >= l then 
            0 
        else 
            let n = countTreesAux (r + p) ((c + q) `mod` w) w l lst in
            if (lst !! r) !! (c `mod` w) == '#' then
                n + 1
            else
                n

day3part1 :: IO ()
day3part1 = do
    s <- readFile "inputs/day3.txt"
    let l = lines s
    print $ countTrees 1 3 l

day3part2 :: IO ()
day3part2 = do
    s <- readFile "inputs/day3.txt"
    let l = lines s
    let ns = map (\(x,y) -> countTrees x y l) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    print $ product ns
