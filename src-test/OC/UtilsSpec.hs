module OC.UtilsSpec where

import OC.Utils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "strBetween" $ do
    it "gets the string between the given characters" $ do
      strBetween '{' '}' "asdf {qwer} asdf" `shouldBe` "qwer"

    it "returns the empty string if either character isn't in the target string" $ do
      strBetween '{' '}' "asdf qwer} asdf" `shouldBe` ""
      strBetween '{' '}' "asdf {qwer asdf" `shouldBe` ""
      strBetween '{' '}' "asdf qwer asdf" `shouldBe` ""
