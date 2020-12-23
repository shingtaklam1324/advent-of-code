module Day8 where

import Data.List.Split
import Data.List.Utils

-- Simple State Machine, 1st num is IX, 2nd num is ACC, 3rd num is the instructions visited
data SM = SM Int Int [Int]

-- Result of the SM after executing one instruction
step :: SM -> (String, Int) -> SM
step (SM ix acc h) ("nop", n) = SM (ix + 1) acc (ix : h)
step (SM ix acc h) ("jmp", n) = SM (ix + n) acc (ix : h)
step (SM ix acc h) ("acc", n) = SM (ix + 1) (acc + n) (ix : h)

run :: [(String, Int)] -> SM -> SM
run l (SM ix acc h) =
    let ins = l !! ix
        (SM ix' acc' h') = step (SM ix acc h) ins
    in if ix' `elem` h' then
        SM ix' acc' h'
    else
        run l (SM ix' acc' h')

parseIns :: String -> (String, Int)
parseIns s = let [s1, s2] = splitOn " " s in (s1, read $ replace "+" "" s2)

day8part1 :: IO ()
day8part1 = do
    s <- readFile "inputs/day8.txt"
    let prog = map parseIns $ lines s
    let (SM ix acc h) = run prog (SM 0 0 [])
    print acc
