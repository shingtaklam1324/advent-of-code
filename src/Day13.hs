module Day13 where

import Data.List.Split
import Data.List
import Data.Bifunctor

day13part1 :: IO ()
day13part1 = do
    [time, buses] <- lines <$> readFile "inputs/day13.txt"
    let t = read time
    let bs = map read $ filter (/= "x") $ splitOn "," buses
    let ts = sortOn snd $ map (\x -> (x, x - t `mod` x)) bs
    let bus = head ts
    print $ uncurry (*) bus

-- day13part2 is the Chinese Remainder Theorem

extendedEuclid :: Integral b => b -> b -> (b, b)
extendedEuclid 1 x = (1, 0)
extendedEuclid x y = let 
    (q, r) = quotRem y x
    (a, b) = extendedEuclid r x in
    (b - a * q, a)

{-
For a,b, p, q, where p, q coprime, finds x such that x mod m = a, x mod n = b
-}
crt :: Integral a => a -> a -> a -> a -> a
crt a b m n =
    let (s, t) = extendedEuclid m n in
    (a * t * n + b * s * m) `mod` (m * n)

day13part2 :: IO ()
day13part2 = do
    [stuff, buses] <- lines <$> readFile "inputs/day13.txt"
    -- Note - We need Integer here as we go over max Int
    -- Note2 - we need b - a `mod` b as we need the time until the next bus
    let b = map ((\ (a, b) -> (b - a `mod` b, b)) . Data.Bifunctor.second read) $ filter (\x -> snd x /= "x") $ zip [0..] $ splitOn "," buses :: [(Integer, Integer)]
    let res = foldl (\(a, m) (b, n) -> (crt a b m n, m * n)) (0, 1) b
    print $ uncurry (+) res `mod` snd res
