module Day2 where


data Rule = Rule {minCount :: Int, maxCount:: Int, requiredChar :: Char} deriving (Show, Eq)



-- should be in the stdlib but what the hey.
splitOnChar :: Char -> String -> (String, String)
splitOnChar c s = (before, after)
    where (before, after, _, _) = foldl accum ("", "", c, False) s
          accum :: (String, String, Char, Bool) -> Char -> (String, String, Char, Bool)
          accum (before, after, needle, alreadySeen) currentChar
            | currentChar == needle = (before, after, needle, True)
            | alreadySeen == True = (before, after ++ [currentChar], needle, alreadySeen)
            | alreadySeen == False = (before ++ [currentChar], after, needle, alreadySeen)

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
