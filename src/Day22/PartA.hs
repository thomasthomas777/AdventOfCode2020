import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

type SetType = S.Set String
type MapType = M.Map String SetType

type Deck = ([Int], [Int])

parseInput :: String -> Deck
parseInput l = let  x = splitOn "\n\n" l
                    a = lines (x !! 0)
                    b = lines (x !! 1)
                    a' = map (\x -> read x :: Int) (tail a)
                    b' = map (\x -> read x :: Int) (tail b)
                        in (a',b')

computeWinning :: [Int] -> Int
computeWinning x = sum $ map (\(a,b) -> a * b) (zip (reverse x) [1..])

process :: Deck -> Int
process ([], []) = error "Equal Draw"
process (x, []) = computeWinning x
process ([], y) = computeWinning y
process ((x:xs), (y:ys)) = if x > y then process (xs ++ [x, y], ys) else process (xs, ys ++ [y, x])

main = do
    contents <- readFile "Input.txt"
    print $ process $ (parseInput contents)