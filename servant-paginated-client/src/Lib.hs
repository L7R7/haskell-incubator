{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib (API, startServer, server, singleClient, paginatedClient) where

import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text hiding (all, find, take)
import Network.Wai.Handler.Warp (run)
import Pagination
import Servant.API
import Servant.Client
import Servant.Server

type API =
  "single-content" :> Get '[JSON] Int
    :<|> "contents" :> QueryParam "page" Int :> Get '[JSON] (Headers '[Header "Link" Text] [Int])

server :: Server API
server =
  pure 5
    :<|> ( \maybePage -> do
             let page = fromMaybe 0 maybePage
                 nextLink =
                   if page < 5
                     then addHeader ("</contents?page=" <> pack (show (page + 1)) <> ">; rel=\"next\"")
                     else noHeader
             pure $ nextLink $ take 10 [(page * 10) ..]
         )

app :: Application
app = serve (Proxy @API) server

startServer :: IO ()
startServer = do
  let port = 8080
  putStrLn $ "Serving endpoint " ++ show port
  run port app

paginatedClientInner :: Maybe Int -> ClientM (Headers '[Header "Link" Text] [Int])
singleClientInner :: ClientM Int
singleClientInner :<|> paginatedClientInner = client (Proxy :: Proxy API)

paginatedClient :: Maybe Int -> ClientM (Headers '[Header "Link" Text] [Int])
paginatedClient = paginated . paginatedClientInner

singleClient :: ClientM Int
singleClient = singleClientInner
