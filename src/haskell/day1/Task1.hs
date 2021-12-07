module Task1 where

main :: IO ()
main = do
    input <- readFile "resources/day1/input.txt"
    let numbers = map (\line -> read line :: Int) (lines input)
    print $ length $ filter (uncurry (>)) (zip (tail numbers) numbers)
