{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiSpec where

import qualified Cib
import Servant
import Servant.Auth
import Servant.Auth.Server
import Servant.Client
import Test.Syd
import Test.Syd.Servant

spec :: Spec
spec = pending "foo"

_ = client (Proxy :: Proxy Cib.AuthAPI)
