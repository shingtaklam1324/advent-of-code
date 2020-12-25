module Day11 where

import qualified Control.Monad as Monad
import Data.List.Utils
import Data.Maybe

getSurrounding :: [[Char]] -> Int -> Int -> [Char]
getSurrounding m r c = Monad.join $ map (drop (c - 1) . take (c + 2)) $ drop (r - 1) $ take (r + 2) m

stepPos :: [[Char]] -> Int -> Int -> Char
stepPos m r c =
    let current = m !! r !! c
        s = getSurrounding m r c
        numOcc = countElem '#' s in
    if current == 'L' && numOcc == 0 then
        '#'
    else if current == '#' && numOcc >= 5 then
        'L'
    else
        current

step :: [[Char]] -> [[Char]]
step m =
    let numRows = length m
        numCols = length $ head m
    in [[stepPos m r c | c <- [0..numCols - 1]] | r <- [0..numRows - 1]]

stepUntilTerm :: [[Char]] -> [[Char]]
stepUntilTerm m = let m' = step m in if m == m' then m' else stepUntilTerm m'

day11part1 :: IO ()
day11part1 = do
    s <- lines <$> readFile "inputs/day11.txt"
    let res = sum $ map (countElem '#') $ stepUntilTerm s
    print res

-- Optimise the stuff below? It's really slow atm, but I guess it works

head' :: [a] -> Maybe a
head' [] = Nothing
head' (h:t) = Just h

getRays :: [[Char]] -> Int -> Int -> [Char]
getRays m r c =
    let numRows = length m
        numCols = length $ head m
        rowL = head' $ filter (/= '.') [m !! r !! c' | c' <- reverse [0..c-1]]
        rowR = head' $ filter (/= '.') [m !! r !! c' | c' <- [c+1..numCols-1]]
        colU = head' $ filter (/= '.') [m !! r' !! c | r' <- reverse [0..r-1]]
        colD = head' $ filter (/= '.') [m !! r' !! c | r' <- [r+1..numRows-1]]
        dUL = head' $ filter (/= '.') [m !! x !! y | x <- reverse [0..r-1], y <- [0..numCols-1], x - y == r - c]
        dBR = head' $ filter (/= '.') [m !! x !! y | x <- [r+1..numRows-1], y <- [0..numCols-1], x - y == r - c]
        dUR = head' $ filter (/= '.') [m !! x !! y | x <- reverse [0..r-1], y <- [0..numCols-1], x + y == r + c]
        dBL = head' $ filter (/= '.') [m !! x !! y | x <- [r+1..numRows-1], y <- [0..numCols-1], x + y == r + c] in
    catMaybes [rowL, rowR, colU, colD, dUL, dUR, dBL, dBR]

stepPos' :: [[Char]] -> Int -> Int -> Char
stepPos' m r c =
    let current = m !! r !! c
        s = getRays m r c
        numOcc = countElem '#' s in
    if current == 'L' && numOcc == 0 then
        '#'
    else if current == '#' && numOcc >= 5 then
        'L'
    else
        current

step' :: [[Char]] -> [[Char]]
step' m =
    let numRows = length m
        numCols = length $ head m
    in [[stepPos' m r c | c <- [0..numCols - 1]] | r <- [0..numRows - 1]]

stepUntilTerm' :: [[Char]] -> [[Char]]
stepUntilTerm' m = let m' = step' m in if m == m' then m' else stepUntilTerm' m'

day11part2 :: IO ()
day11part2 = do
    s <- lines <$> readFile "inputs/day11.txt"
    let res = sum $ map (countElem '#') $ stepUntilTerm' s
    print res
