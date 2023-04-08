module Foo.FooSpec where

import Test.Syd

spec :: Spec
spec = do
  describe "foo" $
    it "foos" $
      2 + 3 == 5
