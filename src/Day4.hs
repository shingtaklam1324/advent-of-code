module Day4 (day4part1, day4part2) where

import Data.List.Utils
import Data.List.Split
import Data.List

{-
For Part 1, we get the keys from each of the entries, and then sort them and
check whether they match one of the two lists (ie ignoring `cid`). If we
assume that all of the keys are correct, then this solution is overcomplicated,
as we don't need to sort, and counting would suffice.
-}
day4part1 :: IO ()
day4part1 = do
    s <- readFile "inputs/day4.txt"
    let validEntries = length $
            filter (\l -> l == ["byr","cid","ecl","eyr","hcl","hgt","iyr","pid"] 
                       || l == ["byr","ecl","eyr","hcl","hgt","iyr","pid"]) $ 
            map (sort . map (head . splitOn ":") . words) $ 
            splitOn "\n\n" s
    print validEntries

validHgt :: String -> Bool
validHgt s
  | "cm" `isInfixOf` s
  = let
      [hs, t] = splitOn "cm" s
      h = read hs
    in 150 <= h && h <= 193
  | "in" `isInfixOf` s
  = let
      [hs, t] = splitOn "in" s
      h = read hs
    in 59 <= h && h <= 76
  | otherwise = False

validField :: String -> Bool
validField ('b':'y':'r':':':t) = (\x -> 1920 <= x && x <= 2002) $ read t
validField ('i':'y':'r':':':t) = (\x -> 2010 <= x && x <= 2020) $ read t
validField ('e':'y':'r':':':t) = (\x -> 2020 <= x && x <= 2030) $ read t
validField ('h':'g':'t':':':t) = validHgt t
validField ('h':'c':'l':':':'#':t) = length t == 6 && all (`elem` "1234567890abcdef") t
validField ('p':'i':'d':':':t) = length t == 9 && all (`elem` "1234567890") t
validField ('c':'i':'d':':':t) = True
validField ('e':'c':'l':':':t) = t `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField _ = False

day4part2 :: IO ()
day4part2 = do
    s <- readFile "inputs/day4.txt"
    let validEntries = 
            length $ 
            filter (\l -> l == ["byr","cid","ecl","eyr","hcl","hgt","iyr","pid"] 
                       || l == ["byr","ecl","eyr","hcl","hgt","iyr","pid"]) $ 
            map (sort . map (head . splitOn ":")) $ 
            filter (all validField) $ 
            map words $ 
            splitOn "\n\n" s
    print validEntries
