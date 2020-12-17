import System.IO
import qualified Data.Map as M
import Data.List.Split (splitOn)

loadInput :: [String] -> Int -> Int -> (M.Map (Int, Int, Int) Bool) -> (M.Map (Int, Int, Int) Bool)
loadInput [] _ _ m = m
loadInput ((x:[]):ys) dx dy m = if x == '#' then (loadInput ys 0 (dy+1) (M.insert (dx,dy,0) True m)) else (loadInput ys 0 (dy+1) m)
loadInput ((x:xs):ys) dx dy m = if x == '#' then (loadInput (xs:ys) (dx+1) dy (M.insert (dx,dy,0) True m)) else (loadInput (xs:ys) (dx+1) dy m)

neighbourCheck :: (Int, Int, Int) -> (M.Map (Int, Int, Int) Bool) -> Int
neighbourCheck (dx, dy, dz) m = let neighboursToCheck = filter (\z -> z /= (dx,dy,dz)) [(dx+n_x, dy+n_y, dz+n_z) | n_x <- [-1..1], n_y <- [-1..1], n_z <- [-1..1]]
                                    in foldl (\x y -> if (M.findWithDefault False y m) then (x+1) else x) 0 neighboursToCheck

currentIterate :: [Int] -> [Int] -> [Int] -> (M.Map (Int, Int, Int) Bool) -> (M.Map (Int, Int, Int) Bool) -> (M.Map (Int, Int, Int) Bool)
currentIterate xs ys zs m n_m = let range = [(x,y,z) | x <- xs, y <- ys, z <- zs] 
                                        in foldl (\x y -> if ((M.findWithDefault False y m) && ((neighbourCheck y m) `elem` [2,3])) then (M.insert y True x) else if (neighbourCheck y m == 3) then (M.insert y True x) else x) n_m range

iterator :: Int -> Int -> (M.Map (Int, Int, Int) Bool) -> [String] -> Int
iterator c t m r
    | c == (t+1) = length $ M.filter (==True) m
    | otherwise  = iterator (c+1) t n_m r
        where r_x = length $ head r
              r_y = length r
              n_x_r = [(0-c)..(r_x+c)] 
              n_y_r = [(0-c)..(r_y+c)] 
              n_z_r = [(0-c)..(0+c)]
              n_m = currentIterate n_x_r n_y_r n_z_r m M.empty

main = do
    contents <- readFile "Input.txt"
    print $ (iterator 1 6 (loadInput (lines contents) 0 0 M.empty) (lines contents))
