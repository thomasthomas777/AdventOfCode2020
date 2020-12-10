import System.IO

data Point = Point Int Int

traverseSlope :: [String] -> Int -> Point -> Int
traverseSlope [[]] t (Point a b) = t
traverseSlope g t (Point a b)
    | b >= maxy  = t
    | currPos == '#'    = traverseSlope g (t+1) newPos
    | otherwise         = traverseSlope g t newPos
    where maxx = length (g !! 0)
          maxy = length g
          currPos = ((g !! b) !! a)
          newPos = Point ((a+3) `mod` (length (g !! 0))) (b+1)

main = do
    contents <- readFile "Input.txt"
    print ( traverseSlope ( lines ( contents ) ) 0 (Point 0 0))
