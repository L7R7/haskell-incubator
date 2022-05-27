module HibWebSpec where

import Hib.Hib (application)
import Network.URI
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Webdriver

spec :: Spec
spec = exampleAppSpec $ do
  it "can navigate to home" $ do
    openPath "/"

exampleAppSpec :: WebdriverSpec () -> Spec
exampleAppSpec = webdriverSpec $ \_ -> do
  app <- liftIO $ application
  portNumber <- applicationSetupFunc app
  let uriStr = "http://127.0.0.1:" <> show portNumber
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure (uri, ())
