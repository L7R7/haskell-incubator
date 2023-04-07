module Main where

import qualified Bar.Spec as Bar
import qualified Foo.Spec as Foo
import Test.Syd

main :: IO ()
main = sydTest $ do
  Foo.spec
  Bar.spec
