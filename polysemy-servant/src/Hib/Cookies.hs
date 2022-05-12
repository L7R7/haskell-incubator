{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | see: https://gist.github.com/ChrisPenner/ca96aea08c9ff9408c6bb36aaddef9ca
module Hib.Cookies (Cookie, CookieVal (..), cookieVal, Cookies, CookieMap (..)) where

import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant
import qualified Web.Cookie as Cookie

-- | Allows deserializing a single cookie in a servant route.
--
-- E.g.
-- @@
--  "my-route"
--  :> Cookie "color-scheme" ColorScheme
--  :> ...
-- @@
--
-- The handler will receive a value of type @Maybe (CookieVal "color-scheme" ColorScheme)@
-- The redundant Maybe wrapper is unfortunate, but can be mostly ignored by using
-- 'cookieVal' as a view pattern when binding.
type Cookie (s :: Symbol) a = Header "Cookie" (CookieVal s a)

-- | The type used to deserialize individual cookie values.
newtype CookieVal (s :: Symbol) a = CookieVal {getCookieVal :: Maybe a}

-- | Elegantly collapses the duplicate Maybe wrapping on a cookie val, use in a view
-- pattern when binding:
--
-- @@
-- useCookieVal :: CookieVal "color-scheme" ColorScheme
-- useCookieVal (cookieVal -> Just ColorScheme) = ...
-- useCookieVal _ = ...
-- @@
cookieVal :: Maybe (CookieVal s a) -> Maybe a
cookieVal mayCV = mayCV >>= getCookieVal

instance (KnownSymbol s, FromHttpApiData a) => FromHttpApiData (CookieVal s a) where
  parseQueryParam _ = error "CookieVal used outside of Header field"
  parseHeader bs =
    parseHeader bs
      & ( \case
            Left txt -> Left txt
            Right (CookieMap m) ->
              case Map.lookup (Text.pack $ symbolVal (Proxy @s)) m of
                Nothing -> Right (CookieVal Nothing)
                Just valTxt -> CookieVal . Just <$> parseQueryParam valTxt
        )

type Cookies = Header "Cookie" CookieMap

-- | This type is used by 'Cookies' and 'CookieVal' as a way to deserialize the Cookie header
-- into a map to be used by a route.
newtype CookieMap = CookieMap {cookieMap :: Map Text Text}

instance FromHttpApiData CookieMap where
  parseQueryParam _ = error "CookieMap used outside of Header field"
  parseHeader bs = Right . CookieMap . Map.fromList $ Cookie.parseCookiesText bs
