module ServerSpec (spec) where

import Lib
import Servant
import Test.Syd
import Test.Syd.Servant

spec :: Spec
spec = do
  it "documents the API in a golden file" $ do
    let apiStructure = layout (Proxy :: Proxy API)
    pureGoldenTextFile "test_resources/api/api-structure.txt" apiStructure
  servantSpec (Proxy :: Proxy API) server $ do
    it "single endpoint" $ do
      res <- singleClient
      liftIO $ res `shouldBe` 5
    it "list endpoint" $ do
      (Headers res _) <- paginatedClient Nothing
      liftIO $ res `shouldBe` [0 .. 59]
