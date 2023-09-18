{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Auth
  ( Login (..),
    LoginRef (..),
    LoginAPI,
    NamedLoginAPI (..),
    loginServer,
    LogoutAPI,
    NamedLogoutAPI (..),
    logoutServer,
    checkCreds,
    loginPage,
    logout,
    loginLink,
    loginFormLink,
    logoutLink,
    WhyIsThisNotUnit,
  )
where

import Config
import Control.Monad.Reader
import qualified Cookies as C
import Data.ByteString hiding (elem)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Text as T hiding (elem)
import Lucid
import Network.HTTP.Types (hLocation)
import Network.URI.Static
import Servant hiding (BasicAuth, NoSuchUser)
import Servant.API.Generic
import Servant.Auth.Server
import Servant.HTML.Lucid
import Servant.Server.Generic (AsServerT)
import User
import Web.FormUrlEncoded

data Login = Login {username :: String, password :: String}
  deriving (Eq, Show, Generic)

instance FromForm Login

type WhyIsThisNotUnit = JSON

data LoginRef = LoggedOut | Denied | BadCreds

instance FromHttpApiData LoginRef where
  parseQueryParam "loggedout" = Right LoggedOut
  parseQueryParam "denied" = Right Denied
  parseQueryParam "badcreds" = Right BadCreds
  parseQueryParam _ = Left "can't parse LoginRef"

instance ToHttpApiData LoginRef where
  toQueryParam LoggedOut = "loggedout"
  toQueryParam Denied = "denied"
  toQueryParam BadCreds = "badcreds"

type LoginAPI = "login" :> NamedRoutes NamedLoginAPI

data NamedLoginAPI mode = NamedLoginAPI
  { loginForm ::
      mode
        :- ReqBody '[FormUrlEncoded] Login
          :> Verb 'POST 302 '[WhyIsThisNotUnit] (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String),
    loginUi ::
      mode
        :- C.Cookie "sbLogin" ByteString
          :> QueryParam "ref" LoginRef
          :> Get '[HTML] (Html ())
  }
  deriving (Generic)

loginServer :: CookieSettings -> JWTSettings -> NamedLoginAPI (AsServerT App)
loginServer cs js =
  NamedLoginAPI
    { loginForm = checkCreds cs js,
      loginUi = loginPage js
    }

type LogoutAPI = "logout" :> NamedRoutes NamedLogoutAPI

newtype NamedLogoutAPI mode = NamedLogoutAPI
  { logoutEndpoint :: mode :- Verb 'POST 302 '[JSON] (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
  }
  deriving (Generic)

logoutServer :: CookieSettings -> NamedLogoutAPI (AsServerT App)
logoutServer cs =
  NamedLogoutAPI
    { logoutEndpoint = logout cs
    }

checkCreds ::
  CookieSettings ->
  JWTSettings ->
  Login ->
  App (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
checkCreds cookieSettings jwtSettings (Login u p) = do
  let lookupResult = M.lookup (u, p) knownUsers
  mApplyCookies <- join <$> traverse (liftIO . acceptLogin cookieSettings jwtSettings) lookupResult
  case mApplyCookies of
    Nothing -> throwError err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just BadCreds))]}
    Just applyCookies -> pure $ addHeader ($$(staticRelativeReference "name" {- todo (linkURI nameLink)-})) $ applyCookies "logged in"

loginPage ::
  JWTSettings ->
  Maybe (C.CookieVal "sbLogin" ByteString) ->
  Maybe LoginRef ->
  App (Html ())
loginPage js authCookie maybeRef = do
  maybeUser <- case authCookie of
    (Just (C.CookieVal (Just bs))) -> liftIO $ verifyJWT @User js bs
    _ -> pure Nothing
  case maybeUser of
    Just _ -> throwError err302 {errHeaders = [(hLocation, toHeader ($$(staticRelativeReference "name" {- todo (linkURI nameLink)-})))]}
    Nothing -> pure $
      html_ $ do
        head_ $ do
          title_ "Login"
        body_ $ do
          h1_ "Login"
          refInfo
          form_ [action_ (toUrlPiece loginFormLink), method_ "POST"] $ do
            div_ $ do
              label_ [for_ "username"] "Username"
              input_ [type_ "username", name_ "username"]
            div_ $ do
              label_ [for_ "password"] "Passwort"
              input_ [type_ "password", name_ "password"]
            div_ $ do
              input_ [type_ "submit", name_ "submit"]
      where
        refInfo =
          traverse_
            ( p_ . \case
                LoggedOut -> "You've been logged out"
                Denied -> "login to get access to this resouce"
                BadCreds -> "the provided credentials are wrong or unknown"
            )
            maybeRef

logout :: CookieSettings -> App (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] [Char])
logout cookieSettings = pure $ addHeader (linkURI $ loginLink (Just LoggedOut)) $ clearSession cookieSettings "logged out"

instance ToHttpApiData URI where
  toUrlPiece = T.pack . show

-- todo
-- nameLink :: Link
-- nameLink = undefined

loginFormLink :: Link
loginLink :: Maybe LoginRef -> Link
(NamedLoginAPI loginFormLink loginLink) = allLinks (Proxy @LoginAPI)

logoutLink :: Link
(NamedLogoutAPI logoutLink) = allLinks (Proxy @LogoutAPI)
