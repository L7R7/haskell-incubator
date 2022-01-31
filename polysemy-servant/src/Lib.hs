{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( someFunc,
  )
where

import Data.Function ((&))
import Network.Wai.Handler.Warp
import Servant

someFunc :: IO ()
someFunc = startServer

type API = "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy

server :: Server API
server = pure "hello, plain IO"

startServer :: IO ()
startServer = serve api server & runSettings (setPort 8080 defaultSettings)
