module Task where
import Data.List.Split (wordsBy)

count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (==x) xs

rpt :: (a -> a) -> a -> Int -> a
rpt _ x 0 = x
rpt f x n = rpt f (f x) (n - 1)

-- NEVER do this
step :: [Int] -> [Int]
step [a0, a1, a2, a3, a4, a5, a6, a7, a8] = [a1, a2, a3, a4, a5, a6, a7+a0, a8, a0]
step _ = []

main :: IO ()
main = do
    input <- readFile "resources/day6/input.txt"
    let numbers = map (\line -> read line :: Int) (wordsBy (==',') input)
    let counts = map (count numbers) [0..8]
    let solution1 = sum $ rpt step counts 80
    let solution2 = sum $ rpt step counts 256
    putStrLn $ "After 80 days: "  ++ show solution1
    putStrLn $ "After 256 days: " ++ show solution2
