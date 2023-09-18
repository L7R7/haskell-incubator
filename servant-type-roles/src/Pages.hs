{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Pages
  ( ProtectedAPI (..),
    protectedServer,
    nameEndpoint,
    foooEndpoint,
    baarEndpoint,
  )
where

import Auth
import Config
import GHC.Generics
import Lucid.Base
import Lucid.Html5
import Network.HTTP.Types (hLocation)
import Servant hiding (BasicAuth, NoSuchUser)
import Servant.Auth
import Servant.Auth.Server
import Servant.HTML.Lucid
import Servant.Server.Generic
import User

data ProtectedAPI auths mode = ProtectedAPI
  { protectedNameEndpoint :: mode :- "name" :> Auth auths User :> Get '[HTML] (Html ()),
    protectedFoooEndpoint :: mode :- "fooo" :> Auth auths (UserHasPermission 'FooPermission) :> Get '[HTML] (Html ()),
    protectedBaarEndpoint :: mode :- "baar" :> Auth auths (UserHasPermission 'BarPermission) :> Get '[HTML] (Html ())
  }
  deriving (Generic)

protectedServer :: ProtectedAPI auths (AsServerT App)
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

nameEndpoint :: User -> App (Html ())
nameEndpoint (User n _ _) = pure $
  html_ $ do
    head_ $ title_ "Welcome"
    body_ $ do
      h1_ "Welcome back!"
      p_ ("Your name is " <> toHtml n)
      p_ $ a_ [href_ "/fooo"] "Try the fooo endpoint (will only work with the FooPermission)"
      p_ $ a_ [href_ "/baar"] "Try the baar endpoint (will only work with the BarPermission)"
      form_ [action_ (toUrlPiece logoutLink), method_ "POST"] $ input_ [type_ "submit", name_ "logout", value_ "logout"]

foooEndpoint :: UserHasPermission 'FooPermission -> App (Html ())
foooEndpoint (UserHasPermission (User n _ _)) = pure $
  html_ $ do
    head_ $ title_ "fooo"
    body_ $ do
      h1_ "Here it foooos! This means you have the FooPermission"
      p_ ("Your name is " <> toHtml n)
      form_ [action_ (toUrlPiece logoutLink), method_ "POST"] $ input_ [type_ "submit", name_ "logout", value_ "logout"]

baarEndpoint :: UserHasPermission 'BarPermission -> App (Html ())
baarEndpoint (UserHasPermission (User n _ _)) = pure $
  html_ $ do
    head_ $ title_ "baar"
    body_ $ do
      h1_ "Here it baars! This means you have the BarPermission"
      p_ ("Your name is " <> toHtml n)
      form_ [action_ (toUrlPiece logoutLink), method_ "POST"] $ input_ [type_ "submit", name_ "logout", value_ "logout"]
