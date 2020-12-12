import System.IO

moveShip :: [String] -> Int -> Int -> Int -> Int
moveShip [] x y deg = (abs x) + (abs y)
moveShip (z:zs) x y deg 
    | dir == 'L' =  moveShip zs x y ((deg - dist) `mod` 360)
    | dir == 'R' =  moveShip zs x y ((deg + dist) `mod` 360)
    | dir == 'N' =  moveShip zs x (y + dist) deg
    | dir == 'S' =  moveShip zs x (y - dist) deg
    | dir == 'E' =  moveShip zs (x + dist) y deg
    | dir == 'W' =  moveShip zs (x - dist) y deg
    | dir == 'F' =  case deg of 
                        0 -> moveShip (("N" ++ tail z) : zs) x y deg
                        90 -> moveShip (("E" ++ tail z) : zs) x y deg
                        180 -> moveShip (("S" ++ tail z) : zs) x y deg
                        270 -> moveShip (("W" ++ tail z) : zs) x y deg
        where dir = head z
              dist = read (tail z) :: Int

main = do
    contents <- readFile "Input.txt"
    print $ moveShip (lines contents) 0 0 90
