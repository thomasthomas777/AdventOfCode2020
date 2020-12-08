import System.IO

calculator :: Char -> Int -> String -> Int
calculator '+' a b = (+) a (read b :: Int)
calculator '-' a b = (-) a (read b :: Int)

parser :: Int -> Int -> [String] -> [Int] -> Int
parser acc index instructions exec
    | index `elem` exec     = acc
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

main = do
    contents <- readFile "Day8Data.txt"
    print $ parser 0 0 (lines $ contents) []