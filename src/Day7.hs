module Day7 (day7part1, day7part2) where

import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.List.Utils
import Data.Maybe

-- Parse split examples -----------------------------------------------------------
-- drab silver * bags contain * no other bags.
-- faded silver * bags contain * 5 mirrored turquoise bags *,* 4 striped purple bags.

{-
To parse a line, there is two cases.
1. the bag contains no other bags
2. the bag does contain other bags.

In the first case, all we need is to parse the name of the bag.

In the second case, we need to parse the name of the bag, and the names and numbers of
all of the bags inside it. To do this, we split the string to get the name of the bag
and the contents. To get the individual contents, we split on the commas, then split on
the spaces. Then the first value is the number, and the second and third are the name
of the bag. As all of the bags have two word names, this is alright.
-}
parseLine :: String -> (String, [(Int, String)])
parseLine l 
  | endswith "no other bags." l =
    let [h1, h2] = splitOn " bags contain " l in (h1, [])
  | otherwise = 
    let [h1, h2] = splitOn " bags contain " l
        h2s = map ((\ x -> let [n, c1, c2] = x in (read n, c1 ++ " " ++ c2)) . take 3 . splitOn " ") $ splitOn ", " h2
    in (h1, h2s)

{-
For efficiency and ease to keep track of the objects, we turn the data from the input
file into a map.
-}
makeMap :: [String] -> Map.Map String [(Int, String)]
makeMap [] = Map.empty
makeMap (h:t) = let (k, v) = parseLine h in Map.insert k v $ makeMap t

{-
We say that we can reach bag a from bag b if we can build a path 
  a -> x -> y -> ... -> z -> b such that 
  x is in Map.lookup a m, y is in Map.lookup x m, ..., b is in Map.lookup z m

Note `t == tgt` is needed here to provide the termination condition, as right now
it is not true that `a` is reachable from `a`.
-}
canReach :: String -> String -> Map.Map String [(Int, String)] -> Bool
canReach src tgt m = any (\(n, t) -> t == tgt || canReach t tgt m) $ fromJust $ Map.lookup src m

{-
Once we have the above, the remainder of the task is fairly simple. All we need to do
is iterate over the keys of the map.

Using memoisation we can achieve a siginificant speed up, but right now it is
not necessary.
-}
day7part1 :: IO ()
day7part1 = do
    s <- readFile "inputs/day7.txt"
    let m = makeMap $ lines s
    let k = filter (\s -> canReach s "shiny gold" m) $ Map.keys m
    print $ length k

{-
We can count the bags contained in each bag recursively
-}
countBags :: String -> Map.Map String [(Int, String)] -> Int
countBags b m = 1 + sum (map (\ (n, t) -> n * countBags t m) $ fromJust $ Map.lookup b m)

day7part2 :: IO ()
day7part2 = do
    s <- readFile "inputs/day7.txt"
    let m = makeMap $ lines s
    let n = countBags "shiny gold" m - 1 -- don't count the shiny gold bag itself
    print n
