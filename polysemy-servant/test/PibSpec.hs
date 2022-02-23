module PibSpec where

import ApiSpecDefinition
import Pib (application)
import Test.Syd
import Test.Syd.Wai

spec :: Spec
spec = waiClientSpecWith application apiSpec
