import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Set (toList, fromList)


splitAndSetifyInput :: String -> [[String]]
splitAndSetifyInput x = map (\y -> splitOn "\n" y) (splitOn "\n\n" x)

intersection :: [String] -> String -> Int
intersection (x:xs) s
    | l == 0    = length $ intersect x s
    | otherwise = intersection xs (intersect s x)
        where l = length xs

count :: [[String]] -> Int
count = foldl (\x y -> ((intersection y (head y)) + x)) 0

main = do
    contents <- readFile "Day6Data.txt"
    print $ count $ splitAndSetifyInput contents
