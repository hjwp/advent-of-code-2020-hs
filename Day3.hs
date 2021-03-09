module Day3 where

spotTrees :: [String] -> [Bool]
spotTrees xs = [c == '#' | c <- rightThreeDownOnes xs]

rightThreeDownOnes :: [String] -> [Char]
rightThreeDownOnes (_:xs) = _rightThreeDownOnes xs 3

_rightThreeDownOnes :: [String] -> Int -> [Char]
_rightThreeDownOnes [] _ = []
_rightThreeDownOnes (x:xs) i = x !! (i `mod` length x): _rightThreeDownOnes xs (i + 3)

main :: IO ()
main = do
    input <- getContents
    print $ length . filter id $ spotTrees $ lines input
