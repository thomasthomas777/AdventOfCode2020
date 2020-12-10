import System.IO

data Point = Point Int Int

traverseSlope :: [String] -> Int -> Point -> Point -> Int
traverseSlope [[]] t (Point a b) (Point c d) = t
traverseSlope g t (Point a b) (Point c d)
    | b >= maxy  = t
    | currPos == '#'    = traverseSlope g (t+1) newPos (Point c d)
    | otherwise         = traverseSlope g t newPos (Point c d)
    where maxx = length (g !! 0)
          maxy = length g
          currPos = ((g !! b) !! a)
          newPos = Point ((a+c) `mod` (length (g !! 0))) (b+d)


traverseIterations :: [String] -> [Point] -> Int
traverseIterations [] [] = 0
traverseIterations g xs = foldl (*) 1 [(traverseSlope g 0 (Point 0 0) p) | p <- xs]

main = do
    contents <- readFile "Input.txt"
    print ( traverseIterations ( lines ( contents ) ) [(Point 1 1), (Point 3 1), (Point 5 1), (Point 7 1), (Point 1 2)])
