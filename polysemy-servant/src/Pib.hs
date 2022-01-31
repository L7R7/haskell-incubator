{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Pib
  ( someFunc,
  )
where

import Control.Exception (try)
import Control.Monad.Trans.Except
import Data.Function ((&))
import Polysemy
import Servant
import Network.Wai.Handler.Warp

someFunc :: IO ()
someFunc = startServer

type API = "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy

hoist :: ServerT API Handler
hoist = hoistServer api liftServer server

server :: ServerT API (Sem r)
server = apiImpl

apiImpl :: Sem r String
apiImpl = pure "hello, polysemy-world"

liftServer :: Sem '[Embed IO] a -> Handler a
liftServer sem = sem & runM & Handler . ExceptT . try

startServer :: IO ()
startServer = serve api hoist & runSettings (setPort 8080 defaultSettings)
