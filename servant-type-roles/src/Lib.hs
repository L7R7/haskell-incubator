{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | very much inspired by https://github.com/haskell-servant/servant-auth/issues/172#issuecomment-680699441
module Lib
  ( logoutLink,
    someFunc,
    application,
  )
where

import Config
import Control.Monad.Reader
import qualified Cookies as C
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString hiding (elem)
import Data.Either.Combinators
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Text as T hiding (elem)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Lucid.Base
import Lucid.Html5
import Network.HTTP.Types (hLocation)
import qualified Network.Wai.Handler.Warp as Wai
import Servant
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Servant.HTML.Lucid
import Servant.Server.Generic (AsServerT)
import Web.FormUrlEncoded

someFunc :: IO ()
someFunc = startServer

class KnownPermission (p :: UserPermission) where
  knownPermission :: Proxy p -> UserPermission

instance KnownPermission 'FooPermission where
  knownPermission _ = FooPermission

instance KnownPermission 'BarPermission where
  knownPermission _ = BarPermission

data UserPermission = FooPermission | BarPermission deriving stock (Eq, Show, Generic)

instance ToJSON UserPermission

instance FromJSON UserPermission

newtype UserHasPermission (p :: UserPermission) = UserHasPermission User deriving (Eq, Show, Generic)

instance FromJSON (UserHasPermission a)

instance ToJSON (UserHasPermission a)

instance ToJWT (UserHasPermission a)

instance (KnownPermission a) => FromJWT (UserHasPermission a) where
  decodeJWT val = do
    usr <- decodeJWT @User val
    maybeToRight "Not Enough Permission" (userHasPermission usr)

userHasPermission :: forall (p :: UserPermission). (KnownPermission p) => User -> Maybe (UserHasPermission p)
userHasPermission usr = if perm `elem` permissions usr then Just (UserHasPermission usr) else Nothing
  where
    perm = knownPermission (Proxy @p)

data User = User {name :: String, email :: String, permissions :: [UserPermission]}
  deriving (Eq, Show, Generic)

instance ToJSON User

instance ToJWT User

instance FromJSON User

instance FromJWT User

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

instance ToHttpApiData URI where
  toUrlPiece = T.pack . show

type API = NamedRoutes NamedAPI

-- | definition of the allowed authentication mechanisms
type Auths = '[Cookie]

data NamedAPI mode = NamedAPI
  { rootRedirect :: mode :- Verb 'GET 302 '[HTML] (Headers '[Header "Location" URI] String),
    protectedApi :: mode :- NamedRoutes (ProtectedAPI Auths),
    loginEndpoints :: mode :- "login" :> NamedRoutes LoginAPI,
    logoutEndpoints :: mode :- "logout" :> NamedRoutes LogoutAPI,
    raw :: mode :- "static" :> Raw
  }
  deriving (Generic)

data ProtectedAPI auths mode = ProtectedAPI
  { protectedNameEndpoint :: mode :- "name" :> Auth auths User :> Get '[HTML] (Html ()),
    protectedFoooEndpoint :: mode :- "fooo" :> Auth auths (UserHasPermission 'FooPermission) :> Get '[HTML] (Html ()),
    protectedBaarEndpoint :: mode :- "baar" :> Auth auths (UserHasPermission 'BarPermission) :> Get '[HTML] (Html ())
  }
  deriving (Generic)

data LoginAPI mode = LoginAPI
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

newtype LogoutAPI mode = LogoutAPI
  { logoutEndpoint :: mode :- Verb 'POST 302 '[JSON] (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
  }
  deriving (Generic)

loginFormLink :: Link
nameLink :: Link
loginLink :: Maybe LoginRef -> Link
logoutLink :: Link
(NamedAPI _ (ProtectedAPI nameLink _ _) (LoginAPI loginFormLink loginLink) (LogoutAPI logoutLink) _) = allFieldLinks

context :: CookieSettings -> JWTSettings -> Context '[CookieSettings, JWTSettings]
context cookieCfg jwtConfig = cookieCfg :. jwtConfig :. EmptyContext

hoistedServer :: CookieSettings -> JWTSettings -> NamedAPI (AsServerT Handler)
hoistedServer cookieSettings jwtSettings = hoistServerWithContext (Proxy @API) (Proxy @'[CookieSettings, JWTSettings]) hoist (server cookieSettings jwtSettings)

hoist :: App a -> Handler a
hoist = flip runReaderT (AppEnv Config) . runApp

instance FromHttpApiData ByteString where
  parseQueryParam = pure . encodeUtf8

server :: CookieSettings -> JWTSettings -> NamedAPI (AsServerT App)
server cs js =
  NamedAPI
    { rootRedirect = redirectRoot,
      protectedApi = protectedServer,
      loginEndpoints =
        LoginAPI
          { loginForm = checkCreds cs js,
            loginUi = loginPage js
          },
      logoutEndpoints =
        LogoutAPI
          { logoutEndpoint = logout cs
          },
      raw = serveDirectoryWebApp "static"
    }

protectedServer :: ProtectedAPI Auths (AsServerT App)
protectedServer =
  ProtectedAPI
    { protectedNameEndpoint = doOrRedirectToLogin nameEndpoint,
      protectedFoooEndpoint = doOrRedirectToLogin foooEndpoint,
      protectedBaarEndpoint = doOrRedirectToLogin baarEndpoint
    }
  where
    doOrRedirectToLogin :: (u -> App x) -> AuthResult u -> App x
    doOrRedirectToLogin f = \case
      Authenticated user -> f user
      _ -> throwError $ err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just Denied))]}

redirectRoot :: App (Headers '[Header "Location" URI] String)
redirectRoot = pure $ addHeader (linkURI nameLink) "root-redirect"

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
    Just _ -> throwError err302 {errHeaders = [(hLocation, toHeader nameLink)]}
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

knownUsers :: M.Map (String, String) User
knownUsers =
  M.fromList
    [ (("AliBaba", "OpenSesame"), User "Ali Baba" "foo" [FooPermission, BarPermission]),
      (("FooUser", "pass123"), User "FooUser" "pass123" [FooPermission]),
      (("BarUser", "pass123"), User "BarUser" "pass123" [BarPermission]),
      (("NooUser", "pass123"), User "NooUser" "pass123" [])
    ]

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
    Just applyCookies -> pure $ addHeader (linkURI nameLink) $ applyCookies "logged in"

logout :: CookieSettings -> App (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] [Char])
logout cookieSettings = pure $ addHeader (linkURI $ loginLink (Just LoggedOut)) $ clearSession cookieSettings "logged out"

nameEndpoint :: User -> App (Html ())
nameEndpoint (User n _ _) = pure $
  html_ $ do
    head_ $ do
      title_ "Welcome"
    body_ $ do
      h1_ "Welcome back!"
      p_ ("Your name is " <> toHtml n)
      p_ $ a_ [href_ "/fooo"] "Try the fooo endpoint (will only work with the FooPermission)"
      p_ $ a_ [href_ "/baar"] "Try the baar endpoint (will only work with the BarPermission)"
      form_ [action_ (toUrlPiece logoutLink), method_ "POST"] $ input_ [type_ "submit", name_ "logout", value_ "logout"]

foooEndpoint :: UserHasPermission 'FooPermission -> App (Html ())
foooEndpoint (UserHasPermission (User n _ _)) = pure $
  html_ $ do
    head_ $ do
      title_ "fooo"
    body_ $ do
      h1_ "Here it foooos! This means you have the FooPermission"
      p_ ("Your name is " <> toHtml n)
      form_ [action_ (toUrlPiece logoutLink), method_ "POST"] $ input_ [type_ "submit", name_ "logout", value_ "logout"]

baarEndpoint :: UserHasPermission 'BarPermission -> App (Html ())
baarEndpoint (UserHasPermission (User n _ _)) = pure $
  html_ $ do
    head_ $ do
      title_ "baar"
    body_ $ do
      h1_ "Here it baars! This means you have the BarPermission"
      p_ ("Your name is " <> toHtml n)
      form_ [action_ (toUrlPiece logoutLink), method_ "POST"] $ input_ [type_ "submit", name_ "logout", value_ "logout"]

startServer :: IO ()
startServer = do
  let port = 8080
  putStrLn $ "Serving endpoint " ++ show port
  app <- application
  Wai.run port app

application :: IO Application
application = do
  let cookieConfig =
        defaultCookieSettings
          { cookieIsSecure = NotSecure,
            cookieMaxAge = Just (secondsToDiffTime 6000),
            cookieSameSite = SameSiteStrict,
            sessionCookieName = "sbLogin",
            cookieXsrfSetting = Just def {xsrfExcludeGet = True}
          }
  jwtSettings <- defaultJWTSettings <$> generateKey
  pure $ serveWithContext (Proxy @API) (context cookieConfig jwtSettings) (hoistedServer cookieConfig jwtSettings)
