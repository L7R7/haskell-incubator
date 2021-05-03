{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

someFunc :: IO ()
someFunc = G.sample twelveDigitGen >>= putStrLn

twelveDigitGen :: Gen String
twelveDigitGen = G.string (R.singleton 12) G.alpha