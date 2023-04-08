module Beep.Boop.BoopSpec where

import Test.Syd

spec :: Spec
spec = do
  describe "beep boop" $
    it "blip" $
      "beep" /= "boop"
