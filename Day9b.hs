import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Set (toList, fromList)

contiguous :: [Int] -> [Int] -> Int -> [Int] -> Int
contiguous xs [] i [] = contiguous xs (tail xs) i []
contiguous xs [] i ys = contiguous (tail xs) (drop 2 xs) i []
contiguous xs (y:ys) i zs
    | a == i    = (foldl1 min (y:zs)) + (foldl1 max (y:zs))
    | a <  i    = contiguous xs ys i (y:zs)
    | otherwise = contiguous (tail xs) (drop 2 xs) i []
        where a = sum zs + y

main = do
    contents <- readFile "Day9Data.txt"
    print $ contiguous (map (\x -> read x :: Int) (lines contents)) [] 50047984 []
