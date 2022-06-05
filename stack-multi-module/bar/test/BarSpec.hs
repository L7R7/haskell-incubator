module BarSpec where

import Bar
import Test.Syd

spec :: Spec
spec = it "bars" $ bar `shouldBe` "bar"
