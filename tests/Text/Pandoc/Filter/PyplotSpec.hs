module Text.Pandoc.Filter.PyplotSpec (
    spec
) where

import qualified Test.Tasty
import Test.Tasty.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Pyplot

spec :: Spec
spec = parallel $ do
    it "should be trivially true" $ do
        True `shouldBe` True
        