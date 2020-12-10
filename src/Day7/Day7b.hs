import System.IO
import Data.List
import Data.Set (toList, fromList)
import Text.Regex.Posix

data BagDependency = BagDependency String [(String, Int)]

parseLine :: String -> String -> [(String, Int)] -> BagDependency
parseLine s m r
    | init_str /= []  =             parseLine (head init_str !! 2) (head init_str !! 1) []
    | empty_bag /= [] =             BagDependency m r
    | dependency_with_carry /= [] = parseLine (head dependency_with_carry !! 4) m new_r
    | dependency_no_carry /= [] =   BagDependency m new_rr
        where init_str = s =~ "^([a-z]+ [a-z]+) bags contain (.*)" :: [[String]]
              empty_bag = s =~ "no other bags." :: [[String]]
              dependency_with_carry = s =~ "([0-9]*) ([a-z]+ [a-z]+) (bag|bags), (.*)" :: [[String]]
              dependency_no_carry = s =~ "([0-9]*) ([a-z]+ [a-z]+) (bag|bags)." :: [[String]]
              new_r = (r ++ [(head dependency_with_carry !! 2, (read (head dependency_with_carry !! 1) :: Int))])
              new_rr = (r ++ [(head dependency_no_carry !! 2, (read (head dependency_no_carry !! 1) :: Int))])

identifyBag :: [BagDependency] -> String -> [(String, Int)]
identifyBag ((BagDependency a b):xs) s
    | a == s    = b
    | otherwise = identifyBag xs s

countCost :: String -> [BagDependency] -> Int
countCost _ [] = 0
countCost s xs = 1 + foldl (\x y -> ((snd y) * (countCost (fst y) xs)) + x) 0 (identifyBag xs s)

main = do
    contents <- readFile "Input.txt"
    print $ (countCost "shiny gold" (map (\x -> parseLine x "" []) (lines contents))) - 1
