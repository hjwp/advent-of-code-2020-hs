-- file Spec.hs
import Test.Hspec
import Day1 (fixExpenses, fixExpenses2)
import Day2 hiding (main)
import Day3 hiding (main)
import qualified Day4

main :: IO ()
main = hspec $ do

  describe "advent of code day 1 expenses thing " $ do
    it "fixExpenses does the simple example correctly" $ do
        let expenses = [1721, 979, 366, 299, 675, 1456]
        fixExpenses expenses `shouldBe` 514579

    it "fixExpenses2 does the simple example correctly" $ do
        let expenses = [1721, 979, 366, 299, 675, 1456]
        fixExpenses2 expenses `shouldBe` 241861950

  describe "split helpers" $ do
    it "splitOnChar should" $ do
        splitOnChar ',' "a,b" `shouldBe` ("a", "b")
        splitOnChar ',' "aa,bb" `shouldBe` ("aa", "bb")
        splitOnChar ',' "a,bb" `shouldBe` ("a", "bb")
        splitOnChar ',' "abb" `shouldBe` ("abb", "")

  describe "advent of code day 2 passwords thing" $ do
    it "parseLine should" $ do
        let r1 = Rule {minCount=1, maxCount=3, requiredChar='a'}
        parseLine "1-3 a: abcde" `shouldBe` (r1, "abcde")
        let r2 = Rule {minCount=3, maxCount=7, requiredChar='b'}
        parseLine "3-7 b: zzzz" `shouldBe` (r2, "zzzz")
        let r3 = Rule {minCount=22, maxCount=77, requiredChar='b'}
        parseLine "22-77 b: zzzz" `shouldBe` (r3, "zzzz")

    it "checkLine should" $ do
        checkLine "1-3 a: abcde" `shouldBe` True

    it "does the simple example correctly" $ do
        let example = ["1-3 a: abcde"
                      ,"1-3 b: cdefg"
                      ,"2-9 c: ccccccccc"
                      ]
        findValidPasswords example `shouldBe` 2

    it "sanity-check a variant of simple example" $ do
        let example = ["2-3 a: abcde"
                      ,"1-3 z: cdefg"
                      ,"2-5 c: cccccc"
                      ]
        findValidPasswords example `shouldBe` 0

  describe "advent of code day 2 passwords thing part 2" $ do

    it "does the simple example correctly" $ do
        checkLine2 "1-3 a: abcde" `shouldBe` True
        checkLine2 "1-3 b: cdefg" `shouldBe` False
        checkLine2 "2-9 c: ccccccccc" `shouldBe` False

  describe "advent of code day 3 maps thing " $ do
    it "should do rightAndDown 3 1" $ do
        rightAndDown 3 1 ["12345", "12345", "12345"] `shouldBe` ['1', '4', '2']
        rightAndDown 3 1 ["abc", "abc", "abc", "abc"] `shouldBe` ['a', 'a', 'a', 'a']

    it "should do rightAndDown 2 2" $ do
        rightAndDown 2 2 ["abcd", "efgh", "jikl", "mnop", "qrst"] `shouldBe` ['a', 'k', 'q']

    it "should handle the simple example" $ do
        let example = ["..##......."
                      ,"#...#...#.."
                      ,".#....#..#."
                      ,"..#.#...#.#"
                      ,".#...##..#."
                      ,"..#.##....."
                      ,".#.#.#....#"
                      ,".#........#"
                      ,"#.##...#..."
                      ,"#...##....#"
                      ,".#..#...#.#"
                      ]

        (rightAndDown 3 1 example) `shouldBe` "..#.##.####"
        (countTrees 1 1 example) `shouldBe` 2
        (countTrees 3 1 example) `shouldBe` 7
        (countTrees 5 1 example) `shouldBe` 3
        (countTrees 7 1 example) `shouldBe` 4
        (countTrees 1 2 example) `shouldBe` 2
        allTrees example `shouldBe` 336

  describe "advent of code day 4 passports thing" $ do

    it "fieldsOf should do sensible things" $ do
        Day4.fieldsOf "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
                \byr:1937 iyr:2017 cid:147 hgt:183cm"
                `shouldBe` Day4.fields

    it "should handle some simple examples" $ do
        let example = 
                "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
                \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
                \\n\
                \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
                \hcl:#cfa07d byr:1929\n\
                \\n\
                \hcl:#ae17e1 iyr:2013\n\
                \eyr:2024\n\
                \ecl:brn pid:760753108 byr:1931\n\
                \hgt:179cm\n\
                \\n\
                \hcl:#cfa07d eyr:2025 pid:166559648\n\
                \iyr:2011 ecl:brn hgt:59in"
        
        (map Day4.isValid $ Day4.separatePasswords example) `shouldBe` [True, False, True, False]

