module Day4 where
import Data.Text (splitOn, pack, unpack)
import Data.List (sort)

fields = sort ["byr"
         ,"iyr"
         ,"eyr"
         ,"hgt"
         ,"hcl"
         ,"ecl"
         ,"pid"
         ,"cid"
         ]

noCountryFields = filter (/= "cid") fields

isValid :: String -> Bool
isValid s = 
    fieldsOf s == fields
    || fieldsOf s == noCountryFields

fieldsOf :: String -> [String]
fieldsOf s = sort $ map head $ map (splitNicelyOn ":") $ words s

-- | splitOn from Data.Text wants "Text" things
-- which you need to convert to + from using pack+unpack
splitNicelyOn :: String -> String -> [String]
splitNicelyOn needle s = map unpack $ splitOn (pack needle) (pack s)

separatePasswords :: String -> [String]
separatePasswords = splitNicelyOn "\n\n"


main :: IO ()
main = do
    input <- getContents
    print $ length . filter isValid . separatePasswords $ input
