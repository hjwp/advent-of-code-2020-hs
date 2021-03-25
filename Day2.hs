module Day2 where
import Data.List (break)


data Rule = Rule {minCount :: Int, maxCount:: Int, requiredChar :: Char}
    deriving (Show, Eq)



-- no stdlib function for this sigh
splitOnChar :: Char -> String -> (String, String)
splitOnChar c s =
    let (before, afterIncludingSep) = break (==c) s
        after = if null afterIncludingSep then [] else tail afterIncludingSep
     in (before, after)

-- | Parse a string of the form "1-3 a"
parseRule :: String -> Rule
parseRule s = Rule (read minCountString) (read maxCountString) rChar
    where (ruleString, [rChar]) = splitOnChar ' ' s
          (minCountString, maxCountString) = splitOnChar '-' ruleString

-- | Parse a line of the form
--   "1-3 a: abcde"
parseLine :: String -> (Rule, String)
parseLine s = (parseRule ruleString, password)
    where (ruleString, _:password) = splitOnChar ':' s

countChars :: Char -> String -> Int
countChars c = length . filter (== c)

checkPassword :: Rule ->  String -> Bool
checkPassword rule string =
    (minCount rule <= charCount) && (charCount <= maxCount rule)
        where charCount = countChars (requiredChar rule) string

checkLine :: String -> Bool
checkLine l =
    checkPassword rule password
    where (rule, password) = parseLine l

findValidPasswords :: [String] -> Int
findValidPasswords s = length $ filter checkLine s

checkLine2 :: String -> Bool
checkLine2 l =
    checkPassword2 rule password
    where (rule, password) = parseLine l

checkPassword2 :: Rule ->  String -> Bool
checkPassword2 (Rule index1 index2 needle) string =
    (char1 == needle || char2 == needle)
    && (char1 /= char2)
        where char1 = string !! (index1 - 1)
              char2 = string !! (index2 - 1)

main :: IO ()
main = do
    input <- getContents
    print $ length . filter checkLine2 $ lines input
