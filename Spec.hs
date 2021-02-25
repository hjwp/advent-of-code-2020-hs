-- file Spec.hs
import Test.Hspec
import Day1 (fixExpenses, fixExpenses2)
import Day2

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
