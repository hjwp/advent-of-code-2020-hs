-- file Spec.hs
import Test.Hspec
import Day1 (fixExpenses, fixExpenses2)
import Day2 (findValidPasswords)

main :: IO ()
main = hspec $ do

  describe "advent of code day 1 expenses thing " $ do
    it "fixExpenses does the simple example correctly" $ do
        let expenses = [1721, 979, 366, 299, 675, 1456]
        fixExpenses expenses `shouldBe` 514579

    it "fixExpenses2 does the simple example correctly" $ do
        let expenses = [1721, 979, 366, 299, 675, 1456]
        fixExpenses2 expenses `shouldBe` 241861950

  describe "advent of code day 2 passwords thing" $ do

    it "does the simple example correctly" $ do
        let example = ["1-3 a: abcde"
                      ,"1-3 b: cdefg"
                      ,"2-9 c: ccccccccc"
                      ]
        findValidPasswords example `shouldBe` 2
