module Advent where

fixExpenses :: [Int] -> Int
fixExpenses xs = head [ x * y | x <- xs, y <- xs, x /= y, x + y == 2020]

main :: IO ()
main = do
    someExpenses <- getContents
    let result = fixExpenses $ map read $ [ x | x <- lines someExpenses, x /= ""]
    print result
