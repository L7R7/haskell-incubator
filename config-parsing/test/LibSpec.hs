{-# LANGUAGE TypeApplications #-}

module LibSpec (spec) where

import Lib
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Config
  goldenSettingsReferenceDocumentationSpec @Config "test_resources/reference.txt" "my-prog"
