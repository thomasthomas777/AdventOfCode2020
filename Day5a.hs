import System.IO

computeRange :: String -> Int -> Int -> Char -> Char -> Int
computeRange "" mi ma b e = min mi ma
computeRange (x:xs) mi ma b e
    | x == b = computeRange xs mi mid b e
    | x == e = computeRange xs (mid + 1) ma b e
        where mid = (mi + ma) `quot` 2

computeRow :: String -> Int
computeRow xs = (computeRange (take 7 xs) 0 127 'F' 'B') * 8 + (computeRange (drop 7 xs) 0 7 'L' 'R')

compute :: [String] -> Int
compute xs = foldr1 (\x y ->if x >= y then x else y) (map computeRow xs)

main = do
    contents <- readFile "Day5Data.txt"
    print $ compute $ lines contents
