module Task2 where
import Data.List.Split (wordsBy)

data Position = P Int Int Int

data Move = Up Int | Down Int | Forward Int

move :: Position -> Maybe Move -> Position
move (P x y aim) (Just (Up      n)) = P x y (aim - n)
move (P x y aim) (Just (Down    n)) = P x y (aim + n)
move (P x y aim) (Just (Forward n)) = P (x+n) (y + aim * n) aim
move p Nothing = p

parseMove :: String -> Maybe Move
parseMove = makeMove . wordsBy (==' ')
  where makeMove :: [String] -> Maybe Move
        makeMove ["up",      x] = Just $ Up      (read x)
        makeMove ["down",    x] = Just $ Down    (read x)
        makeMove ["forward", x] = Just $ Forward (read x)
        makeMove _ = Nothing

mult :: Position -> Int
mult (P x y _) = x * y

-- Probably missing some idiomatic Haskell thing here
main :: IO ()
main = do
    input <- readFile "resources/day2/input.txt"
    let moves = map parseMove (lines input)
    let finalPos = foldl move (P 0 0 0) moves
    print $ mult finalPos
