-- file Spec.hs
import Test.Hspec
import Day1 (fixExpenses)

main :: IO ()
main = hspec $ do

  describe "Day1 day 1 expenses thing " $ do
    it "does the simple example correctly" $ do
        let expenses = [1721, 979, 366, 299, 675, 1456]
        fixExpenses expenses `shouldBe` 514579
