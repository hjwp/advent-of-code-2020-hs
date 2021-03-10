module Day3 where

import Data.List (mapAccumL)

allTrees :: [String] -> Int
allTrees xs = foldr (*) 1 [countTrees x y xs | (x, y) <- [(1,1), (3,1), (5,1), (7,1), (1,2)]]

countTrees :: Int -> Int -> [String] -> Int
countTrees across down = length . spotTrees across down

spotTrees :: Int -> Int -> [String] -> [Bool]
spotTrees across down xs = [True | c <- rightAndDown across down xs, c == '#']

everyNth n [] = []
everyNth n xs = head xs : everyNth n (drop n xs)

rightAndDown :: Int -> Int -> [String] -> [Char]
rightAndDown across down =
    let doStep n row = (n + across, row !! (n `mod` length row))
    in snd . mapAccumL doStep 0 . everyNth down

main :: IO ()
main = do
    input <- getContents
    print $ allTrees . lines $ input
