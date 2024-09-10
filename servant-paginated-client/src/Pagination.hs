{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Pagination (paginated) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (local)
import qualified Data.ByteString.UTF8 as BS
import Data.Foldable
import Data.Text hiding (all, find, take)
import GHC.TypeLits (Symbol)
import qualified Network.HTTP.Client.Internal as Http
import Network.HTTP.Link
import Servant.API
import Servant.Client

-- see: https://stackoverflow.com/a/78954717/5247502
paginated ::
  forall (s :: Symbol) rest a.
  (Monoid a) =>
  ClientM (Headers (Header s Text ': rest) a) ->
  ClientM (Headers (Header s Text ': rest) a)
paginated initial = do
  let overrideUrl :: URI -> ClientM b -> ClientM b
      overrideUrl uri action = do
        let transformClientRequest original =
              original {Http.path = BS.fromString (uriPath uri), Http.queryString = BS.fromString (uriQuery uri)}
            transformMakeClientRequest f host servantReq = do
              httpReq <- f host servantReq
              pure $ transformClientRequest httpReq
            transformClientEnv clientEnv =
              clientEnv {makeClientRequest = transformMakeClientRequest (makeClientRequest clientEnv)}
        local transformClientEnv action

      go :: ClientM (Headers (Header s Text ': rest) a) -> a -> ClientM (Headers (Header s Text ': rest) a)
      go action acc = do
        r <- action
        let acc' = acc <> getResponse r
            HCons header _ = getHeadersHList r
        case header of
          UndecodableHeader {} -> do
            liftIO $ throwIO $ userError "undecodable header"
          MissingHeader -> do
            pure $ r {getResponse = acc'}
          Header next -> do
            let maybeNextLink = do
                  linkHeaders <- parseLinkHeader next
                  nextLink <- find (all (\tpl -> tpl == (Rel, "next")) . linkParams) linkHeaders
                  pure $ href nextLink
            case maybeNextLink of
              Just nextLink -> go (overrideUrl nextLink initial) acc'
              Nothing -> pure $ r {getResponse = acc'}
  go initial mempty
