determineKey :: Int -> Int -> Int -> Int -> Int
determineKey ck l c dk
    | match = foldl (\x y -> (dk * x) `mod` 20201227) 1 [1..c]
    | otherwise = determineKey ck ((7 * l) `mod` 20201227) (c+1) dk
        where match = ck == l

main = do
    print $ determineKey 14012298 1 0 74241