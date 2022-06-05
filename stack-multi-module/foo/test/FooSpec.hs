module FooSpec where

import Foo
import Test.Syd

spec :: Spec
spec = it "foos" $ foo `shouldBe` "barbaz"
