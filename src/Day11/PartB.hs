import System.IO

seenSeat :: [String] -> Int -> Int -> Int -> Int -> Bool
seenSeat g x y dx dy 
    | ny >= max_y = False
    | nx >= max_x = False
    | ny < 0 = False
    | nx < 0 = False
    | curr_seat == '#' = True
    | curr_seat == 'L' = False
    | otherwise = seenSeat g nx ny dx dy
        where   nx = (+) x dx
                ny = (+) y dy
                max_x = length $ head g
                max_y = length g
                curr_seat = g !! ny !! nx

calc :: [String] -> [String] -> Int -> Int -> [String]
calc orig_g new_grid x y
    | y >= max_y         = init new_grid
    | x >= max_x         = calc orig_g (new_grid ++ [[]]) 0 (y+1)
    | curr_seat == '.'   = calc orig_g (init new_grid ++ [(last new_grid ++ ".")]) (x+1) y
    | otherwise          = calc orig_g (init new_grid ++ [(last new_grid ++ [refreshed_grid_elem])]) (x+1) y
        where max_x = length $ head orig_g
              max_y = length orig_g
              curr_seat = orig_g !! y !! x
              u = seenSeat orig_g x y 0 (-1)
              d = seenSeat orig_g x y 0 1
              r = seenSeat orig_g x y 1 0
              l = seenSeat orig_g x y (-1) 0
              ul = seenSeat orig_g x y (-1) (-1)
              ur = seenSeat orig_g x y 1 (-1)
              dl = seenSeat orig_g x y (-1) 1
              dr = seenSeat orig_g x y 1 1
              sum' = sum $ map fromEnum [u, d, r, l, ul, ur, dl, dr]

              refreshed_grid_elem = if (curr_seat == '#' && sum' >= 5) then 'L' else if (curr_seat == 'L' && sum' == 0) then '#' else curr_seat

stabaliseGrid :: [String] -> Int
stabaliseGrid g
    | g == next_calc = foldl (\x y -> x + (length $ filter (=='#') y)) 0 next_calc
    | otherwise      = stabaliseGrid next_calc
        where next_calc = calc g [[]] 0 0

main = do
    contents <- readFile "Input.txt"
    print $ stabaliseGrid (lines contents)
