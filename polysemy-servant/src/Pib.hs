{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Pib
  ( someFunc,
  )
where

import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.Time
import Debug.Trace (trace)
import Network.Wai.Handler.Warp
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Servant
import Servant.API.Generic (Generic)
import Servant.Auth
import Servant.Auth.Server
import Web.FormUrlEncoded

someFunc :: IO ()
someFunc = startServer

data User = User {name :: String, email :: String}
  deriving (Eq, Show, Generic)

data Login = Login {username :: String, password :: String}
  deriving (Eq, Show, Generic)

instance ToJSON User

instance ToJWT User

instance FromJSON User

instance FromJWT User

instance ToJSON Login

instance FromJSON Login

instance FromForm Login

type Unprotected =
  "login"
    :> ReqBody '[FormUrlEncoded] Login
    :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)

type Logout = "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)

type Protected = "name" :> Get '[JSON] String

type API = (Auth '[Cookie] User :> Protected) :<|> Unprotected :<|> Logout

api :: Proxy API
api = Proxy

context :: CookieSettings -> JWTSettings -> Context '[CookieSettings, JWTSettings]
context cookieCfg jwtConfig = cookieCfg :. jwtConfig :. EmptyContext

hoist :: CookieSettings -> JWTSettings -> ServerT API Handler
hoist cookieSettings jwtSettings = hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) liftServer (server cookieSettings jwtSettings)

liftServer :: Sem '[Error ServerError, Embed IO] a -> Handler a
liftServer sem =
  sem
    & runError @ServerError
    & runM
    & Handler . ExceptT

server :: (Member (Embed IO) r, Member (Error ServerError) r) => CookieSettings -> JWTSettings -> ServerT API (Sem r)
server cs js = nameEndpoint :<|> checkCreds cs js :<|> logout cs

checkCreds ::
  (Member (Embed IO) r, Member (Error ServerError) r) =>
  CookieSettings ->
  JWTSettings ->
  Login ->
  Sem r (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
checkCreds cookieSettings jwtSettings Login {username = "AliBaba", password = "OpenSesame"} = do
  mApplyCookies <- embed $ acceptLogin cookieSettings jwtSettings (User "Ali Baba" "foo")
  case mApplyCookies of
    Nothing -> throw err401
    Just applyCookies -> pure $ applyCookies "logged in"
checkCreds _ _ Login {username = user} = trace ("Received " ++ user) $ throw err401

logout :: CookieSettings -> Sem r (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] [Char])
logout cookieSettings = pure $ clearSession cookieSettings "logged out"

nameEndpoint :: (Member (Error ServerError) r) => AuthResult User -> Sem r [Char]
nameEndpoint (Authenticated (User n _)) = pure n
nameEndpoint x = trace ("Access Denied " ++ show x) $ throw err401

startServer :: IO ()
startServer = do
  let port = 8080
  let cookieConfig =
        defaultCookieSettings
          { cookieIsSecure = NotSecure,
            cookieMaxAge = Just (secondsToDiffTime 60),
            cookieSameSite = SameSiteStrict,
            sessionCookieName = "sbLogin",
            cookieXsrfSetting = Just def {xsrfExcludeGet = True}
          }
  jwtSettings <- defaultJWTSettings <$> generateKey
  putStrLn $ "Serving endpoint " ++ show port
  serveWithContext api (context cookieConfig jwtSettings) (hoist cookieConfig jwtSettings) & runSettings (setPort port defaultSettings)
