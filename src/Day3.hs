module Day3 (day3part1, day3part2) where

-- change in row, change in col, data, num trees
countTrees :: Int -> Int -> [[Char]] -> Int
countTrees p q lst = 
    -- `w` represents the width of the input, and we need this as we may go
    -- past the first map, and we will need to take the column `mod w`
    let w = length $ head lst
        l = length lst in
    -- and we just past these into the auxillary function
    countTreesAux p q w l lst
  where
    countTreesAux :: Int -> Int -> Int -> Int -> [[Char]] -> Int
    countTreesAux r c w l lst = 
        -- If we have gone past `l`, then we have reached the bottom of the map,
        -- and we should terminate.
        if r >= l then 
            0
        else
            -- Otherwise, check the number of trees in the rest of the path
            let n = countTreesAux (r + p) ((c + q) `mod` w) w l lst in
            -- and check if the current point is a tree
            if (lst !! r) !! (c `mod` w) == '#' then
                n + 1
            else
                n

-- Using the functions above, part 1 is simple.
day3part1 :: IO ()
day3part1 = do
    s <- readFile "inputs/day3.txt"
    let l = lines s
    print $ countTrees 1 3 l

-- Part 2 just requires passing in different inputs to the function above.
day3part2 :: IO ()
day3part2 = do
    s <- readFile "inputs/day3.txt"
    let l = lines s
    let ns = map (\(x,y) -> countTrees x y l) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    print $ product ns
