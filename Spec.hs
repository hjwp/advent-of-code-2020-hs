-- file Spec.hs
import Test.Hspec
import Advent

main :: IO ()
main = hspec $ do

  describe "Advent day 1 expenses thing " $ do
    it "does the simple example correctly" $ do
        let expenses = [1721, 979, 366, 299, 675, 1456]
        fixExpenses expenses `shouldBe` 514579
