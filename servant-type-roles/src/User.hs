{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module User
  ( User (..),
    UserHasPermission (..),
    UserPermission (..),
    knownUsers,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Either.Combinators
import qualified Data.Map as M
import Servant hiding (BasicAuth, NoSuchUser)
import Servant.API.Generic
import Servant.Auth.Server

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

type instance BasicAuthCfg = ()

instance FromBasicAuthData User where
  fromBasicAuthData (BasicAuthData user pass) () = pure $ maybe NoSuchUser Authenticated (M.lookup (show user, show pass) knownUsers)

instance (KnownPermission a) => FromBasicAuthData (UserHasPermission a) where
  fromBasicAuthData (BasicAuthData user pass) () = pure $ maybe NoSuchUser Authenticated maybeResult
    where
      maybeResult = M.lookup (show user, show pass) knownUsers >>= userHasPermission

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

knownUsers :: M.Map (String, String) User
knownUsers =
  M.fromList
    [ (("AliBaba", "OpenSesame"), User "Ali Baba" "foo" [FooPermission, BarPermission]),
      (("FooUser", "pass123"), User "FooUser" "pass123" [FooPermission]),
      (("BarUser", "pass123"), User "BarUser" "pass123" [BarPermission]),
      (("NooUser", "pass123"), User "NooUser" "pass123" [])
    ]
