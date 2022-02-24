{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hib.Docs
  ( writeDocs,
  )
where

import Hib.Hib
import Lucid.Base
import Servant
import Servant.Auth.Docs ()
import Servant.Auth.Server
import Servant.Docs hiding (API)
import qualified Servant.Docs as D
import Web.Cookie
import Web.FormUrlEncoded

apiDocs :: D.API
apiDocs = docs (Proxy :: Proxy API)

writeDocs :: IO ()
writeDocs = writeFile "tmp.md" (markdown apiDocs)

instance ToSample (Html ()) where
  toSamples _ = noSamples

instance ToSample SetCookie where
  toSamples _ = singleSample defaultSetCookie

instance {-# OVERLAPPING #-} ToSample String where
  toSamples _ = singleSample "/login?loggedout"

instance ToParam (QueryParam' '[Optional, Strict] "ref" LoginRef) where
  toParam _ = DocQueryParam "ref" ["loggedout", "denied"] "This signals where a redirect came from to the login page" Normal

instance ToForm Login

instance ToSample Login where
  toSamples _ = singleSample $ Login "example-user" "example-password"

instance ToSample URI where
  toSamples _ = noSamples
