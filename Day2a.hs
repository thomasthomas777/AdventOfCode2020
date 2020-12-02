import System.IO

strToInt :: String -> Int
strToInt a = read a :: Int

minVal :: String -> String -> Int
minVal "" a = 0
minVal (i:is) a
    | i == '-'  = strToInt a
    | otherwise = minVal is (a ++ [i])

maxVal :: String -> String -> Bool -> Int
maxVal "" _ _ = 0
maxVal (i:is) a s
    | i == '-'      = maxVal is a True
    | i == ' '      = strToInt a
    | s == False    = maxVal is a s
    | otherwise     = maxVal is (a ++ [i]) s

charCheck :: String -> Bool -> Char 
charCheck "" _ = ' '
charCheck (i:is) s
    | i == ' '      = charCheck is True
    | s == True     = i
    | otherwise     = charCheck is False

stringCheck :: String -> Bool -> String
stringCheck "" _ = " "
stringCheck (i:is) s
    | i == ':'     = stringCheck is True
    | s == True    = is -- Leading space
    | otherwise    = stringCheck is False

reduceString :: String -> Char -> Int
reduceString "" _ = 0
reduceString i c = length [x | x <- i, x == c]

verifyString :: Int -> Int -> Char -> String -> Bool
verifyString mi ma c s = (reduceString s c) >= mi && (reduceString s c) <= ma

validPassword :: String -> Bool
validPassword [] = False
validPassword i 
    | verifyString mi ma c s == True    = True
    | otherwise                         = False
        where mi = minVal i ""
              ma = maxVal i "" False
              c = charCheck i False
              s = stringCheck i False
              

conquerer :: [String] -> Int -> Int
conquerer [] v = v
conquerer (i:is) v
    | r == True  = conquerer is v+1
    | otherwise  = conquerer is v
        where r = validPassword i

main = do
    contents <- readFile "Day2Data.txt"
    print ( conquerer ( lines ( contents ) ) 0)
