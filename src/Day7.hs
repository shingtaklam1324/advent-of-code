module Day7 where

import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.List.Utils
import Data.Maybe

-- drab silver * bags contain * no other bags.
-- faded silver * bags contain * 5 mirrored turquoise bags *,* 4 striped purple bags.

parseLine :: String -> (String, [(Int, String)])
parseLine l 
  | endswith "no other bags." l =
    let [h1, h2] = splitOn " bags contain " l in (h1, [])
  | otherwise = 
    let [h1, h2] = splitOn " bags contain " l
        h2s = map ((\ x -> let [n, c1, c2] = x in (read n, c1 ++ " " ++ c2)) . take 3 . splitOn " ") $ splitOn ", " h2
    in (h1, h2s)

makeMap :: [String] -> Map.Map String [(Int, String)]
makeMap [] = Map.empty
makeMap (h:t) = let (k, v) = parseLine h in Map.insert k v $ makeMap t

canReach :: String -> String -> Map.Map String [(Int, String)] -> Bool
canReach src tgt m = any (\(n, t) -> canReach t tgt m || t == tgt) $ fromJust $ Map.lookup src m

day7part1 :: IO ()
day7part1 = do
    s <- readFile "inputs/day7.txt"
    let m = makeMap $ lines s
    let k = filter (\s -> canReach s "shiny gold" m) $ Map.keys m
    print $ length k

countBags :: String -> Map.Map String [(Int, String)] -> Int
countBags b m = 1 + sum (map (\ (n, t) -> n * countBags t m) $ fromJust $ Map.lookup b m)

day7part2 :: IO ()
day7part2 = do
    s <- readFile "inputs/day7.txt"
    let m = makeMap $ lines s
    let n = countBags "shiny gold" m - 1 -- don't count the shiny gold bag itself
    print n
