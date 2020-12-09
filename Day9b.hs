import System.IO

contiguous :: [Int] -> [Int] -> Int -> [Int] -> Int
contiguous xs [] i [] = contiguous xs (tail xs) i []
contiguous xs [] i ys = contiguous (tail xs) (drop 2 xs) i []
contiguous xs (y:ys) i zs
    | a == i    = (foldl1 min new_zs) + (foldl1 max new_zs)
    | a <  i    = contiguous xs ys i new_zs
    | otherwise = contiguous (tail xs) (drop 2 xs) i []
        where a = sum new_zs
              new_zs = y:zs

main = do
    contents <- readFile "Day9Data.txt"
    print $ contiguous (map (\x -> read x :: Int) (lines contents)) [] 50047984 []
