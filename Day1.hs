module Day1 where

fixExpenses :: [Int] -> Int
fixExpenses xs = head [ x * y | x <- xs, y <- xs, x /= y, x + y == 2020]

fixExpenses2 :: [Int] -> Int
fixExpenses2 xs = head [ 
    x * y * z | x <- xs, y <- xs, z <- xs
   , x /= y
   , y /= z
   , x + y + z == 2020
                       ]

main :: IO ()
main = do
    someExpenses <- getContents
    let result = fixExpenses2 $ map read $ [ x | x <- lines someExpenses, x /= ""]
    print result
