{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Cib where

import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (secondsToDiffTime)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Web.FormUrlEncoded

data User = User {name :: String, email :: String}
  deriving (Eq, Show,  Generic)

instance ToJSON User

instance ToJWT User

instance FromJSON User

instance FromJWT User

data Login = Login {username :: String, password :: String}
  deriving (Eq, Show, Generic)

instance ToJSON Login

instance FromJSON Login

instance FromForm Login

type Unprotected =
  "login"
    :> ReqBody '[FormUrlEncoded] Login
    :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type Protected = "name" :> Get '[JSON] String

type AuthAPI =
  (Servant.Auth.Server.Auth '[Cookie] User :> Protected)
    :<|> Unprotected

type API auths = (Servant.Auth.Server.Auth auths User :> Protected) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts = protected :<|> checkCreds cs jwts

cookieConfig :: CookieSettings
cookieConfig =
  defaultCookieSettings
    { cookieIsSecure = NotSecure,
      cookieMaxAge = Just (secondsToDiffTime 60),
      cookieSameSite = SameSiteStrict,
      sessionCookieName = "sbLogin",
      cookieXsrfSetting = Just def {xsrfExcludeGet = True}
    }

getJwtConfig :: IO JWTSettings
getJwtConfig = defaultJWTSettings <$> generateKey

context :: CookieSettings -> JWTSettings -> Context '[CookieSettings, JWTSettings]
context cookieCfg jwtConfig = cookieCfg :. jwtConfig :. EmptyContext

protected :: Servant.Auth.Server.AuthResult User -> Server Protected
protected (Servant.Auth.Server.Authenticated (User n _)) = return n
protected x = trace ("Access Denied " ++ show x) $ throwAll err401

checkCreds ::
  CookieSettings ->
  JWTSettings ->
  Login ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
checkCreds cookieSettings jwtSettings Login {username = "AliBaba", password = "OpenSesame"} = do
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (User "Ali Baba" "foo")
  case mApplyCookies of
    Nothing -> trace "Nothing" $ throwError err401
    Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ Login {username = user, password = _} =
  trace
    ("Received " ++ user)
    throwError
    err401

someFunc :: IO ()
someFunc = do
  let port = 8080
  jwtConfig <- getJwtConfig
  putStrLn $ "Serving endpoint " ++ show port
  run port $ serveWithContext (Proxy :: Proxy AuthAPI) (context cookieConfig jwtConfig) (server cookieConfig jwtConfig)
