import System.IO
import Control.Lens

calc :: [String] -> [String] -> Int -> Int -> [String]
calc orig_g new_grid x y
    | y >= max_y         = init new_grid
    | x >= max_x         = calc orig_g (new_grid ++ [[]]) 0 (y+1)
    | curr_seat == '.'   = calc orig_g (init new_grid ++ [(last new_grid ++ ".")]) (x+1) y
    | otherwise          = calc orig_g (init new_grid ++ [(last new_grid ++ [refreshed_grid_elem])]) (x+1) y
        where max_x = length $ head orig_g
              max_y = length orig_g
              curr_seat = orig_g !! y !! x
              neighbours = [
                  (x-1, y+1), (x, y+1), (x+1, y+1),
                  (x-1, y),             (x+1, y),
                  (x-1, y-1), (x, y-1), (x+1, y-1)]
              sum' = length $ filter (==True) $ map (\y -> case orig_g ^? element (snd y) . element (fst y) of {Just x -> (x == '#'); Nothing -> False}) neighbours
              refreshed_grid_elem = if (curr_seat == '#' && sum' >= 4) then 'L' else if (curr_seat == 'L' && sum' == 0) then '#' else curr_seat

stabaliseGrid :: [String] -> Int
stabaliseGrid g
    | g == next_calc = foldl (\x y -> x + (length $ filter (=='#') y)) 0 next_calc
    | otherwise      = stabaliseGrid next_calc
        where next_calc = calc g [[]] 0 0

main = do
    contents <- readFile "Input.txt"
    print $ stabaliseGrid (lines contents)
