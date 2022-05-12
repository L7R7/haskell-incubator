{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Cib (someFunc, AuthAPI, application) where

import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (secondsToDiffTime)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Web.FormUrlEncoded

data User = User {name :: String, email :: String}
  deriving (Eq, Show, Generic)

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

type Logout = "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)

type Protected = "name" :> Get '[JSON] String

type AuthAPI = (Servant.Auth.Server.Auth '[Cookie] User :> Protected) :<|> Unprotected :<|> Logout

server :: CookieSettings -> JWTSettings -> Server AuthAPI
server cs jwts = protected :<|> checkCreds cs jwts :<|> logout cs

logout :: CookieSettings -> Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
logout cookieSettings = pure $ clearSession cookieSettings "logged out"

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
protected _ = throwAll err401

checkCreds ::
  CookieSettings ->
  JWTSettings ->
  Login ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
checkCreds cookieSettings jwtSettings Login {username = "AliBaba", password = "OpenSesame"} = do
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (User "Ali Baba" "foo")
  case mApplyCookies of
    Nothing -> throwError err401
    Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = throwError err401

application :: IO Application
application = do
  jwtConfig <- getJwtConfig
  pure $ serveWithContext (Proxy :: Proxy AuthAPI) (context cookieConfig jwtConfig) (server cookieConfig jwtConfig)

someFunc :: IO ()
someFunc = do
  let port = 8080
  putStrLn $ "Serving endpoint " ++ show port
  app <- application
  run port app
