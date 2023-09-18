{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib
  ( logoutLink,
    startServer,
    application,
  )
where

import Auth
import Config
import Control.Monad.Reader
import Data.ByteString hiding (elem)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import qualified Network.Wai.Handler.Warp as Wai
import Pages
import Servant hiding (BasicAuth, NoSuchUser)
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Servant.HTML.Lucid
import Servant.Server.Generic (AsServerT)

type API = NamedRoutes NamedAPI

-- | definition of the allowed authentication mechanisms
type Auths = '[BasicAuth, Cookie]

data NamedAPI mode = NamedAPI
  { rootRedirect :: mode :- Verb 'GET 302 '[HTML] (Headers '[Header "Location" URI] String),
    protectedApi :: mode :- NamedRoutes (ProtectedAPI Auths),
    loginEndpoints :: mode :- LoginAPI,
    logoutEndpoints :: mode :- LogoutAPI,
    raw :: mode :- "static" :> Raw
  }
  deriving (Generic)

nameLink :: Link
(NamedAPI _ (ProtectedAPI nameLink _ _) (NamedLoginAPI _ _) (NamedLogoutAPI _) _) = allFieldLinks

instance FromHttpApiData ByteString where
  parseQueryParam = pure . encodeUtf8

server :: CookieSettings -> JWTSettings -> NamedAPI (AsServerT App)
server cs js =
  NamedAPI
    { rootRedirect = redirectRoot,
      protectedApi = protectedServer,
      loginEndpoints = loginServer cs js,
      logoutEndpoints = logoutServer cs,
      raw = serveDirectoryWebApp "static"
    }

redirectRoot :: App (Headers '[Header "Location" URI] String)
redirectRoot = pure $ addHeader (linkURI nameLink) "root-redirect"

startServer :: IO ()
startServer = do
  let port = 8080
  putStrLn $ "Serving endpoint " ++ show port
  app <- application
  Wai.run port app

application :: IO Application
application = do
  jwtSettings <- defaultJWTSettings <$> generateKey
  let cookieConfig =
        defaultCookieSettings
          { cookieIsSecure = NotSecure,
            cookieMaxAge = Just (secondsToDiffTime 6000),
            cookieSameSite = SameSiteStrict,
            sessionCookieName = "sbLogin",
            cookieXsrfSetting = Just def {xsrfExcludeGet = True}
          }
      context :: Context '[CookieSettings, JWTSettings, ()]
      context = cookieConfig :. jwtSettings :. () :. EmptyContext
      hoistedServer :: NamedAPI (AsServerT Handler)
      hoistedServer = hoistServerWithContext (Proxy @API) (Proxy @'[CookieSettings, JWTSettings, ()]) hoist (server cookieConfig jwtSettings)
      hoist :: App a -> Handler a
      hoist = flip runReaderT (AppEnv Config) . runApp

  pure $ serveWithContext (Proxy @API) context hoistedServer
