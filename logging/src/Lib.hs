{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( someFunc,
  )
where

import Polysemy
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log
import Polysemy.Log.Colog
import Polysemy.Resource
import Polysemy.Time (interpretTimeGhc)

someFunc :: IO ()
someFunc =
  runFinal
    . embedToFinal @IO
    . interpretTimeGhc
    . runResource
    . interpretCologConcNative
    . interpretLogColog
    $ prog

prog :: Member Log r => Sem r ()
prog = do
  Log.debug "debugging"
  Log.info "info"
  Log.error "failing"
