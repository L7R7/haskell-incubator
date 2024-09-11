{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pagination (DecorateApi, WrapPagination (..)) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (local)
import qualified Data.ByteString.UTF8 as BS
import Data.Foldable
import Data.Text hiding (all, find, take)
import GHC.Base (Symbol, Type)
import GHC.Generics
import qualified Network.HTTP.Client.Internal as Http
import Network.HTTP.Link
import Servant
import Servant.Client

type family DecorateApi a :: Type where
  DecorateApi ((sym :: Symbol) :> rest) = sym :> DecorateApi rest
  DecorateApi (first :> rest) = first :> DecorateApi rest
  DecorateApi (left :<|> right) = DecorateApi left :<|> DecorateApi right
  -- String is actually [Char]
  DecorateApi (Get contentTypes String) = Get contentTypes String
  DecorateApi (Get contentTypes [res]) = AddPagination (Get contentTypes [res])
  DecorateApi (Get contentTypes res) = Get contentTypes res

type family AddPagination a :: Type where
  AddPagination (Get contentTypes [res]) = QueryParam "page" Int :> Get contentTypes (Headers '[Header "Link" Text] [res])

class WrapPagination a where
  wrapPagination :: a -> a

instance {-# OVERLAPPABLE #-} WrapPagination (ClientM a) where
  wrapPagination = id

instance {-# OVERLAPPABLE #-} WrapPagination (a -> ClientM b) where
  wrapPagination f = wrapPagination . f

instance {-# OVERLAPPING #-} WrapPagination (ClientM (Headers '[Header "Link" Text] [a])) where
  wrapPagination = paginated

instance {-# OVERLAPPING #-} WrapPagination (a -> ClientM (Headers '[Header "Link" Text] [b])) where
  wrapPagination f = paginated . f

instance (WrapPagination left, WrapPagination right) => WrapPagination (left :<|> right) where
  wrapPagination (left :<|> right) = wrapPagination left :<|> wrapPagination right

instance
  ( Generic (api (AsClientT ClientM)),
    GServantProduct (Rep (api (AsClientT ClientM))),
    WrapPagination (ToServant api (AsClientT ClientM))
  ) =>
  WrapPagination (api (AsClientT ClientM))
  where
  wrapPagination = fromServant . wrapPagination . toServant

-- see: https://stackoverflow.com/a/78954717/5247502
paginated ::
  forall (s :: Symbol) rest a.
  (Monoid a) =>
  ClientM (Headers (Header s Text ': rest) a) ->
  ClientM (Headers (Header s Text ': rest) a)
paginated initial = go initial mempty
  where
    overrideUrl :: URI -> ClientM b -> ClientM b
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
