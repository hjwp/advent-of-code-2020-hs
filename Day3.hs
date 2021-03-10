module Day3 where

allTrees :: [String] -> Int
allTrees xs = foldr (*) 1 $ [countTrees x y xs | (x, y) <- [(1,1), (3,1), (5,1), (7,1), (1,2)]]

countTrees :: Int -> Int -> [String] -> Int
countTrees across down xs = length . filter id $ spotTrees across down xs

spotTrees :: Int -> Int -> [String] -> [Bool]
spotTrees across down xs = [c == '#' | c <- rightAndDown across down xs 0]

rightAndDown :: Int -> Int -> [String] -> Int -> [Char]
rightAndDown _ _ [] _ = []
rightAndDown across down (x:xs) i = x !! (i `mod` length x): rightAndDown across down xs (i + across)

main :: IO ()
main = do
    input <- getContents
    print $ allTrees $ lines input
