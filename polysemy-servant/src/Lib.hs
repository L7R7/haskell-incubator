{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Lib (someFunc) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map as M
import Data.Proxy
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import System.IO

port :: Int
port = 8080

data AuthenticatedUser = AUser
  { auID :: Int,
    auOrgID :: Int
  }
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

type Login = ByteString

type Password = ByteString

type DB = Map (Login, Password) AuthenticatedUser

type Connection = DB

type Pool a = a

initConnPool :: IO (Pool Connection)
initConnPool =
  pure $
    fromList
      [ (("user", "pass"), AUser 1 1),
        (("user2", "pass2"), AUser 2 1)
      ]

authCheck ::
  Pool Connection ->
  BasicAuthData ->
  IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData login password) =
  pure $
    maybe SAS.Indefinite Authenticated $ M.lookup (login, password) connPool

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type TestAPI =
  "foo" :> Capture "i" Int :> Get '[JSON] ()
    :<|> "bar" :> Get '[JSON] ()

type TestAPIServer = Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> TestAPI

server :: Server TestAPIServer
server (Authenticated user) = handleFoo :<|> handleBar
  where
    handleFoo :: Int -> Handler ()
    handleFoo n = liftIO $ hPutStrLn stderr $ concat ["foo: ", show user, " / ", show n]
    handleBar :: Handler ()
    handleBar = pure ()
server SAS.BadPassword = throwAll err401
server SAS.NoSuchUser = throwAll err401
server Indefinite = throwAll err401

mkApp :: Pool Connection -> IO Application
mkApp connPool = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connPool
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy TestAPIServer
  pure $ serveWithContext api cfg server

main :: IO ()
main = do
  connPool <- initConnPool
  let settings = setPort port $ setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp connPool

someFunc :: IO ()
someFunc = main
