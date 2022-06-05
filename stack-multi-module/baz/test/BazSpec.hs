module BazSpec where

import Baz
import Test.Syd

spec :: Spec
spec = it "bazs" $ baz `shouldBe` "baz"
