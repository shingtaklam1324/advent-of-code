module Day8 where

import Data.List.Split
import Data.List.Utils

{-
For Day 8, the situation can be simply handled by designing an interpreter for the
assembly language.
-}

-- Simple State Machine, 1st num is IX, 2nd num is ACC, 3rd num is the instructions visited
data SM = SM Int Int [Int] deriving Show

{-
The result of executing one instruction on the state machine
-}
step :: SM -> (String, Int) -> SM
step (SM ix acc h) ("nop", n) = SM (ix + 1) acc (ix : h)
step (SM ix acc h) ("jmp", n) = SM (ix + n) acc (ix : h)
step (SM ix acc h) ("acc", n) = SM (ix + 1) (acc + n) (ix : h)

{-
Running the state machine, and returning the final state when it terminates. It also returns
whether it terminated due to a loop (False) or whether it ran out of instructions to
execute (True)
-}
run :: [(String, Int)] -> SM -> (SM, Bool)
run l (SM ix acc h) =
    let ins = l !! ix
        (SM ix' acc' h') = step (SM ix acc h) ins
    in if ix' `elem` h' then
        (SM ix' acc' h', False)
    else if length l <= ix' then
        (SM ix' acc' h', True)
    else
        run l (SM ix' acc' h')

{-
Parsing each line of the input into an instruction. `read` does not parse "+123" correctly,
so we remove the "+" before parsing.
-}
parseIns :: String -> (String, Int)
parseIns s = let [s1, s2] = splitOn " " s in (s1, read $ replace "+" "" s2)

{-
Then the first part of day 8 can be done by running the input
-}
day8part1 :: IO ()
day8part1 = do
    s <- readFile "inputs/day8.txt"
    let prog = map parseIns $ lines s
    let (SM ix acc h, False) = run prog (SM 0 0 [])
    print acc

{-
For part 8 of day 8, we generate all of the variant programs which change exactly
one of the "jmp" or "nop".
-}
variantProgs :: [(String, Int)] -> [[(String, Int)]]
variantProgs (("nop", n) : t) = (("jmp", n) : t) : map (\ l -> ("nop", n) : l) (variantProgs t)
variantProgs (("jmp", n) : t) = (("nop", n) : t) : map (\ l -> ("jmp", n) : l) (variantProgs t)
variantProgs [] = []
variantProgs (h : t) = map (h :) $ variantProgs t

{-
Then for part 2, we run all of the variant programs and check which one terminates
by running out of instructions to execute.
-}
day8part2 :: IO ()
day8part2 = do
    s <- readFile "inputs/day8.txt"
    let prog = map parseIns $ lines s
    let progs = variantProgs prog
    let res = map (\m -> run m (SM 0 0 [])) progs
    let [(SM ix acc h, True)] = filter snd res
    print acc
