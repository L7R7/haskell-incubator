{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Data.Time (UTCTime)
import Debug.Trace
import Polysemy
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds), Time, interpretTimeGhcAt, mkDatetime, year)
import qualified Polysemy.Time as Time

testTime :: UTCTime
testTime =
  mkDatetime 1845 12 31 23 59 59

someFunc :: IO ()
someFunc = do
  now <- runM $ interpretTimeGhcAt testTime Time.now
  print now
  print testTime
  print $ now == testTime -- returns False, was expecting True
