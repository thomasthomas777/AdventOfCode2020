import System.IO

calculator :: Char -> Int -> String -> Int
calculator '+' a b = (+) a (read b :: Int)
calculator '-' a b = (-) a (read b :: Int)

parser :: Int -> Int -> [String] -> [Int] -> Int
parser acc index instructions exec
    | index `elem` exec     = -1
    | the_end               = acc
    | nop_op == True        = parser acc new_index instructions new_exec
    | acc_op == True        = parser new_acc new_index instructions new_exec
    | jmp_op == True        = parser acc new_index_jumped instructions new_exec
        where   the_end = index >= length instructions
                curr_line = instructions !! index
                nop_op = (take 4 curr_line) == "nop "
                acc_op = (take 4 curr_line) == "acc "
                jmp_op = (take 4 curr_line) == "jmp "
                new_index = index + 1
                new_index_jumped = calculator (curr_line !! 4) index (drop 5 curr_line)
                new_exec = exec ++ [index] 
                new_acc = calculator (curr_line !! 4) acc (drop 5 curr_line)
                new_line = instructions !! new_index
                new_line_jumped = instructions !! new_index_jumped
                repeat_line_acc = calculator (curr_line !! 4) 5 (drop 5 curr_line)

modifyLine :: [String] -> Int -> [String]
modifyLine l i
    | is_acc == True = l
    | is_nop == True = (take i l) ++ ["jmp" ++ (drop 3 curr_line)] ++ (drop (1+i) l)
    | is_jmp == True = (take i l) ++ ["nop" ++ (drop 3 curr_line)] ++ (drop (1+i) l)
        where curr_line = l !! i
              is_acc = (take 3 curr_line) == "acc"
              is_jmp = (take 3 curr_line) == "jmp"
              is_nop = (take 3 curr_line) == "nop"

modifyInstructions :: [String] -> Int -> Int
modifyInstructions xs index
    | xs == mod_xs  = modifyInstructions xs (index+1)
    | runval /= -1  = runval
    | runval == -1  = modifyInstructions xs (index+1)
        where   mod_xs = modifyLine xs index
                runval = parser 0 0 mod_xs []

main = do
    contents <- readFile "Day8Data.txt"
    print $ modifyInstructions (lines contents) 1