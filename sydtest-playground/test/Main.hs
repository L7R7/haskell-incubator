module Main where

import qualified Bar.Spec as Bar
import qualified Beep.Boop.Spec as Beep
import qualified Foo.Spec as Foo
import Test.Syd

main :: IO ()
main = sydTest $ do
  Foo.spec
  beforeAll (pure "bar") Bar.spec
  Beep.spec
