module Day3 where

countTrees :: Int -> Int -> [String] -> Int
countTrees across down xs = length . filter id $ spotTrees across down xs

spotTrees :: Int -> Int -> [String] -> [Bool]
spotTrees across down xs = [c == '#' | c <- rightAndDown across down xs]

rightAndDown :: Int -> Int -> [String] -> [Char]
rightAndDown across down  (_:xs) = _rightAndDown across down xs across

_rightAndDown :: Int -> Int -> [String] -> Int -> [Char]
_rightAndDown _ _ [] _ = []
_rightAndDown across down (x:xs) i = x !! (i `mod` length x): _rightAndDown across down xs (i + across)

main :: IO ()
main = do
    input <- getContents
    print $ countTrees 3 1 $ lines input
