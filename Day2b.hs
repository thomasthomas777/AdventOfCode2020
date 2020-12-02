import System.IO
import Text.Regex.Posix

data ParsedComponents = ParsedComponents Int Int Char String

strToInt :: String -> Int
strToInt a = read a :: Int

verifyString :: Int -> Int -> Char -> String -> Bool
verifyString mi ma c s = ((s !! (mi - 1)) == c) /= ((s !! (ma - 1)) == c)

validPassword :: ParsedComponents -> Bool
validPassword (ParsedComponents mi ma c s)
    | verifyString mi ma c s == True    = True
    | otherwise                         = False
              
matchLine :: String -> [String]
matchLine "" = [""]
matchLine a = (a =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: [[String]]) !! 0

parsed :: [String] -> ParsedComponents
parsed xs = ParsedComponents (strToInt (xs!!1)) (strToInt (xs!!2)) (((xs!!3) !! 0)) (xs!!4)

conquerer :: [String] -> Int -> Int
conquerer [] v = v
conquerer (i:is) v
    | r == True  = conquerer is v+1
    | otherwise  = conquerer is v
        where r = validPassword (parsed (matchLine i))
main = do
    contents <- readFile "Day2Data.txt"
    print ( conquerer ( lines ( contents ) ) 0)
