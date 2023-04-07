module Bar.BarSpec where

import Test.Syd

spec :: Spec
spec = do
  describe "bar bla" $
    it "bars" $
      "foo" /= "bar"
