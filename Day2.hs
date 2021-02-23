module Day2 where
import Data.Text (splitOn)


data Rule = Rule {minCount :: Int, maxCount:: Int, char :: Char}

parseLine :: String -> (Rule, String)
parseLine s = 
    let firstSplit = splitOn ": " s
        ruleString = head firstSplit
        passwordString = last firstSplit
        ruleDetails  = splitOn " " ruleString
        minmax = head ruleDetails
        char = last ruleDetails
        minmaxes = splitOn "-" minmax
        minCountString = head minmaxes
        maxCountString = last minmaxes in
        (Rule (read minCountString) (read maxCountString) char, passwordString)


findValidPasswords :: [String] -> Int
findValidPasswords strings = head $ map read strings

main :: IO ()
main = do
    print "hello"
