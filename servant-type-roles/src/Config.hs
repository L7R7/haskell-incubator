{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Config (App (..), AppEnv (..), Config (..), mkConfig) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Servant (Handler, ServerError)

newtype App a = App
  { runApp :: ReaderT AppEnv Handler a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppEnv,
      MonadError ServerError
    )

newtype AppEnv = AppEnv
  { config :: Config
  }

data Config = Config

mkConfig :: IO Config
mkConfig = pure Config
