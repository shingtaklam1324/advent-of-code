module Day12 where

data Boat = Boat {
    pos :: (Int, Int),
    dir :: (Int, Int)
} deriving Show

vadd :: Num a => (a, a) -> (a, a) -> (a, a)
vadd (x, y) (u, v) = (x + u, y + v)

{-

 (1, 0) -> (0, 1)
 (0, 1) -> (-1, 0)

 so matrix is
 | 0 -1 |
 | 1 0  |

 Then | 0 -1 | | x | = | -y |
      | 1  0 | | y |   | x  |
-}
rot :: Num a => (a, a) -> (a, a)
rot (x, y) = (-y, x)

smul :: Num a => a -> (a, a) -> (a, a)
smul k (x, y) = (k*x, k*y)

step :: Boat -> (Char, Int) -> Boat
step b ('N', n) = b { pos = pos b `vadd` (0, n) }
step b ('S', n) = b { pos = pos b `vadd` (0, -n) }
step b ('E', n) = b { pos = pos b `vadd` (n, 0) }
step b ('W', n) = b { pos = pos b `vadd` (-n, 0) }
step b ('L', n) = b { dir = iterate rot (dir b) !! (n `div` 90) }
step b ('R', n) = b { dir = iterate rot (dir b) !! ((360 - n) `div` 90) }
step b ('F', n) = b { pos = pos b `vadd` (n `smul` dir b) }

day12part1 :: IO ()
day12part1 = do
    s <- map (\x -> (head x, read $ drop 1 x :: Int)) . lines <$> readFile "inputs/day12.txt"
    let res = foldl step (Boat { pos = (0,0), dir = (1, 0) }) s
    print $ abs (fst . pos $ res) + abs (snd . pos $ res)

step' :: Boat -> (Char, Int) -> Boat
step' b ('N', n) = b { dir = dir b `vadd` (0, n) }
step' b ('S', n) = b { dir = dir b `vadd` (0, -n) }
step' b ('E', n) = b { dir = dir b `vadd` (n, 0) }
step' b ('W', n) = b { dir = dir b `vadd` (-n, 0) }
step' b ('L', n) = b { dir = iterate rot (dir b) !! (n `div` 90) }
step' b ('R', n) = b { dir = iterate rot (dir b) !! ((360 - n) `div` 90) }
step' b ('F', n) = b { pos = pos b `vadd` (n `smul` dir b) }

day12part2 :: IO ()
day12part2 = do
    s <- map (\x -> (head x, read $ drop 1 x :: Int)) . lines <$> readFile "inputs/day12.txt"
    let res = foldl step' (Boat { pos = (0,0), dir = (10, 1) }) s
    print $ abs (fst . pos $ res) + abs (snd . pos $ res)
