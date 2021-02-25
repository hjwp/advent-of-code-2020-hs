import Text.Parsec
-- import Day2 (Rule)

data Rule = Rule {minCount :: Int, maxCount:: Int, requiredChar :: Char} deriving Show

-- | Parse a line of the form
--   "1-3 a: abcde"
parseLine :: Parsec String () (Rule, String)
parseLine = do
    min <- fmap read $ many digit
    char '-'
    max <- fmap read $ many digit
    char ' '
    requiredChar <- anyChar
    string ": "
    password <- many alphaNum
    return (Rule min max requiredChar, password)

main :: IO ()
main = do
    print $ parse parseLine "(no source file)" "1-3 a: abcde"

