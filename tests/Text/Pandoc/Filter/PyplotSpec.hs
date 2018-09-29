module Text.Pandoc.Filter.PyplotSpec (
    spec
) where

import Test.Tasty.Hspec

spec :: Spec
spec = parallel $ do
    it "should be trivially true" $ do
        True `shouldBe` True
        