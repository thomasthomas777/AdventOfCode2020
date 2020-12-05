import System.IO
import Data.List

computeRange :: String -> Int -> Int -> Char -> Char -> Int
computeRange "" mi ma b e = min mi ma
computeRange (x:xs) mi ma b e
    | x == b = computeRange xs mi mid b e
    | x == e = computeRange xs (mid + 1) ma b e
        where mid = (mi + ma) `quot` 2

computeRow :: String -> Int
computeRow xs = (computeRange (take 7 xs) 0 127 'F' 'B') * 8 + (computeRange (drop 7 xs) 0 7 'L' 'R')

computeSeat :: [Int] -> Int -> Int
computeSeat [] a = 0
computeSeat x (-1) = computeSeat x (head x)
computeSeat (x:xs) a 
    | z = x + 1
    | otherwise = computeSeat xs x
        where z = (x + 2 == head xs)

compute :: [String] -> Int
compute xs = computeSeat ( sort $ map computeRow xs ) (-1)

main = do
    contents <- readFile "Day5Data.txt"
    print $ compute $ lines contents
