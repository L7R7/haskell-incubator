module ServerSpec (spec) where

import Lib
import Pagination
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
    it "list endpoint (full content)" $ do
      (Headers res _) <- paginatedClient Nothing Nothing
      liftIO $ res `shouldBe` [0 .. 59]
    it "list endpoint (with limit)" $ do
      (Headers res _) <- paginatedClient Nothing (Just (PaginationConfig 10))
      liftIO $ res `shouldBe` [0 .. 9]
    it "list endpoint (with limit higher than available elements)" $ do
      (Headers res _) <- paginatedClient Nothing (Just (PaginationConfig 100))
      liftIO $ res `shouldBe` [0 .. 59]
    it "list endpoint (with 0 as limit)" $ do
      (Headers res _) <- paginatedClient Nothing (Just (PaginationConfig 0))
      liftIO $ res `shouldBe` []
    it "list endpoint (with negative limit)" $ do
      (Headers res _) <- paginatedClient Nothing (Just (PaginationConfig (-5)))
      liftIO $ res `shouldBe` []
