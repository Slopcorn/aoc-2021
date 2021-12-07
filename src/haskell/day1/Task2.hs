module Task2 where

main :: IO ()
main = do
    input <- readFile "resources/day1/input.txt"
    let numbers = map (\line -> read line :: Int) (lines input)
    let sumsOfThree = zipWith3 (\a b c -> a + b + c) numbers (tail numbers) (tail $ tail numbers)
    print $ length $ filter (uncurry (>)) (zip (tail sumsOfThree) sumsOfThree)
