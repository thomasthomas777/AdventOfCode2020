import System.IO

moveShip :: [String] -> Int -> Int -> Int -> Int -> Int
moveShip [] x y wx wy = (abs x) + (abs y)
moveShip (z:zs) x y wx wy 
    | dir == 'L' =  moveShip zs x y (fst new_w_l) (snd new_w_l)
    | dir == 'R' =  moveShip zs x y (fst new_w_r) (snd new_w_r)
    | dir == 'N' =  moveShip zs x y wx (wy+dist)
    | dir == 'S' =  moveShip zs x y wx (wy-dist)
    | dir == 'E' =  moveShip zs x y (wx+dist) wy
    | dir == 'W' =  moveShip zs x y (wx-dist) wy
    | dir == 'F' =  moveShip zs (x + (wx*dist)) (y + (wy*dist)) wx wy 
        where dir = head z
              dist = read (tail z) :: Int
              new_w_l = foldl (\x y -> ((snd x) * (-1), fst x)) (wx, wy) [1..((dist `quot` 90) `mod` 4)]
              new_w_r = foldl (\x y -> (snd x, (fst x) * (-1))) (wx, wy) [1..((dist `quot` 90) `mod` 4)]

main = do
    contents <- readFile "Input.txt"
    print $ moveShip (lines contents) 0 0 10 1
