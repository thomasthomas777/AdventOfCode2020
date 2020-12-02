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

reduceString :: String -> Char -> Int -> Int -> Bool
reduceString "" _ _ _ = False
reduceString (s:ss) c cn it
    | cn == it  = c == s
    | otherwise = reduceString ss c cn (it+1)


verifyString :: Int -> Int -> Char -> String -> Bool
verifyString mi ma c s = (reduceString s c mi 1) /= (reduceString s c ma 1)

validPassword :: String -> Bool
validPassword [] = False
validPassword i 
    | verifyString mi ma c s == True    = True
    | otherwise                         = False
        where mi = minVal i ""
              ma = maxVal i "" False
              c = charCheck i False
              s = stringCheck i False
              

conquerer :: (Eq a, Num a) => [String] -> a -> a
conquerer [] v = v
conquerer (i:is) v
    | r == True  = conquerer is v+1
    | otherwise  = conquerer is v
        where r = validPassword i

main = do
    contents <- readFile "Day2Data.txt"
    print ( conquerer ( lines ( contents ) ) 0)
