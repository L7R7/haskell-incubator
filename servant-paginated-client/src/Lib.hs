{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib (API, startServer, server, singleClient, paginatedClient) where

import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text hiding (all, find, take)
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Pagination
import Servant
import Servant.Client

data NamedAPI mode = NamedAPI
  { singleContent :: mode :- DecorateApi ("single-content" :> Get '[JSON] Int),
    paginatedContent :: mode :- DecorateApi ("contents" :> Get '[JSON] [Int]),
    barApi :: mode :- "nested" :> NamedRoutes BarAPI
  }
  deriving stock (Generic)

data BarAPI mode = BarAPI
  { foo :: mode :- DecorateApi ("foo" :> Get '[JSON] Text),
    bar :: mode :- DecorateApi ("bar" :> Get '[JSON] [Text])
  }
  deriving stock (Generic)

type API = NamedRoutes NamedAPI

server :: Server API
server =
  NamedAPI
    { singleContent = pure 5,
      paginatedContent =
        \maybePage -> do
          let page = fromMaybe 0 maybePage
              nextLink =
                if page < 5
                  then addHeader ("</contents?page=" <> pack (show (page + 1)) <> ">; rel=\"next\"")
                  else noHeader
          pure $ nextLink $ take 10 [(page * 10) ..],
      barApi =
        BarAPI
          { foo = pure "foo",
            bar = \_ -> pure $ noHeader ["bar", "baz"]
          }
    }

app :: Application
app = serve (Proxy @API) server

startServer :: IO ()
startServer = do
  let port = 8080
  putStrLn $ "Serving endpoint " ++ show port
  run port app

clientRecord :: NamedAPI (AsClientT ClientM)
clientRecord = wrapPagination $ client (Proxy :: Proxy API)

paginatedClient :: Maybe Int -> Maybe PaginationConfig -> ClientM (Headers '[Header "Link" Text] [Int])
paginatedClient = paginatedContent clientRecord

singleClient :: ClientM Int
singleClient = singleContent clientRecord
