import System.IO
import Data.List.Split (splitOn)

type Deck = ([Int], [Int])

parseInput :: String -> Deck
parseInput l = let  x = splitOn "\n\n" l
                    a = lines (x !! 0)
                    b = lines (x !! 1)
                    a' = map (\x -> read x :: Int) (tail a)
                    b' = map (\x -> read x :: Int) (tail b)
                        in (a', b')

computeWinning :: [Int] -> Int
computeWinning x = sum $ zipWith (*) (reverse x) [1..]

process :: Deck -> [Deck] -> (Int, Int)
process ([], []) a = error "Equal Draw"
process (x, []) a = (1, computeWinning x)
process ([], y) a = (2, computeWinning y)
process ((x:xs), (y:ys)) a
    | seen                              = (1, computeWinning (x:xs))
    | (new && w == 1) || (not new && r) = process (xs ++ [x, y], ys) new_deck
    | (new && w == 2) || not r          = process (xs, ys ++ [y, x]) new_deck
        where  r = x > y 
               curr_deck = ((x:xs), (y:ys))
               seen = curr_deck `elem` a
               new_deck = curr_deck:a
               new = (length xs >= x) && (length ys >= y)
               (w, _) = process (take x xs, take y ys) []

main = do
    contents <- readFile "Input.txt"
    print $ snd (process (parseInput contents) [])
