module CibSpec where

import ApiSpecDefinition
import Cib (application)
import Test.Syd
import Test.Syd.Wai

spec :: Spec
spec = waiClientSpecWith application apiSpec
