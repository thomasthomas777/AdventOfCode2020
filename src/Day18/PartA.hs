import System.IO

processRightParenthesis :: [String] -> [String] -> ([String], [String])
processRightParenthesis i (j:js)
    | j == "(" = (i, js)
    | otherwise = processRightParenthesis (j:i) js

rpnConvertor :: [String] -> [String] -> [String] -> [String]
rpnConvertor [] num stack = (reverse num) ++ stack
rpnConvertor (i:is) num stack 
    | length stack == 0     = rpnConvertor is num (i:stack)
    | i == "("              = rpnConvertor is num (i:stack) 
    | i == ")"              = rpnConvertor is (fst processRightParenthesis') (snd processRightParenthesis')
    | i `elem` ["+", "*"]   = if (head stack == "(") then rpnConvertor is num (i:stack) else rpnConvertor is ((head stack):num) (i:(tail stack))
    | otherwise             = rpnConvertor is (i:num) stack
        where processRightParenthesis' = processRightParenthesis num stack

rpnEvaluator :: [String] -> [String] -> Int
rpnEvaluator [] stack = read (head stack) :: Int
rpnEvaluator (i:is) stack 
    | length stack == 0     = rpnEvaluator is (i:stack)
    | i == "*"              = rpnEvaluator is (mult':new_stack)
    | i == "+"              = rpnEvaluator is (add':new_stack)
    | otherwise             = rpnEvaluator is (i:stack)
        where   
                fst_elem_int = read (stack !! 0) :: Int
                snd_elem_int = read (stack !! 1) :: Int
                new_stack = drop 2 stack
                mult' = show $ fst_elem_int * snd_elem_int
                add'  = show $ fst_elem_int + snd_elem_int


main = do
    contents <- readFile "Input.txt"
    print $ sum (map (\x -> (rpnEvaluator (rpnConvertor x [] []) [])) (map (\z -> foldl (\x y -> if y == ' ' then x else x ++ [[y]]) [] z) (lines contents)))
