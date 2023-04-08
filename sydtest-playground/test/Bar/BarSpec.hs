{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Bar.BarSpec where

import Test.Syd

spec :: TestDef (String : otherOuters) ()
spec = do
  describe "bar bla" $
    itWithOuter "bars" $ \s ->
      s `shouldBe` "bar"
