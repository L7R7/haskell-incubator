{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
-- TODO: check this?
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Lib where

import Barbies
import Control.Applicative
import Control.Lens
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Barbie
import Data.Foldable (fold)
import Data.Functor.Compose (Compose (..))
import Data.List.NonEmpty
import Data.Text.Lens
import Data.Validation (Validation (..))
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

someFunc :: IO ()
someFunc = pure ()

data Options_ f = Options_
  { serverHost :: f String,
    numThreads :: f Int,
    verbosity :: f Int
  }
  deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (AllBF Show f Options_) => Show (Options_ f)

deriving instance (AllBF Eq f Options_) => Eq (Options_ f)

deriving instance (AllBF A.FromJSON f Options_) => A.FromJSON (Options_ f)

readEnv :: Read a => String -> (IO `Compose` Maybe) a
readEnv envKey = Compose $ do
  lookupEnv envKey
    >>= pure . \case
      Just x -> readMaybe x
      Nothing -> Nothing

-- envOpts :: Options_ ??
-- envOpts =
--     Options_
--     { serverHost = readEnv "SERVER_HOST"
--     , numThreads = readEnv "NUM_THREADS"
--     , verbosity  = pure Nothing -- Don't read verbosity from environment
--     }

envOpts :: Options_ (IO `Compose` Maybe)
envOpts =
  Options_
    { serverHost = readEnv "SERVER_HOST",
      numThreads = readEnv "NUM_THREADS",
      verbosity = Compose $ pure Nothing -- Don't read verbosity from environment
    }

jsonOptsDerived :: A.Value -> Options_ Maybe
jsonOptsDerived = fromResult . A.fromJSON
  where
    fromResult (A.Success a) = a
    fromResult (A.Error _) = buniq Nothing

jsonOptsCustom :: A.Value -> Options_ Maybe
jsonOptsCustom =
  bsequence
    Options_
      { serverHost = findField $ key "host" . _String . unpacked,
        numThreads = findField $ key "num_threads" . _Number . to round,
        verbosity = findField $ key "verbosity" . _Number . to round
      }
  where
    findField :: Fold A.Value a -> Compose ((->) A.Value) Maybe a
    findField p = Compose (preview p)

-- bsequence :: (Applicative f, TraversableB b) => b (Compose f g) -> f (b g)
-- bsequence :: Options_ (Compose ((->) A.Value) Maybe) -> (A.Value -> Options_ Maybe)

envOpts' :: IO (Options_ Maybe)
envOpts' =
  bsequence
    Options_
      { -- serverHost is already a string so we don't need to 'read' it.
        serverHost = Compose . lookupEnv $ "SERVER_HOST",
        numThreads = readEnv "NUM_THREADS",
        -- We can 'ignore' a field by simply returning Nothing.
        verbosity = Compose . pure $ Nothing
      }

readConfigFileDummy :: IO A.Value
readConfigFileDummy = pure $ A.object ["host" A..= A.String "example.com", "verbosity" A..= A.Number 42]

getOptions :: IO (Options_ Maybe)
getOptions = do
  configJson <- readConfigFileDummy
  envOpts'' <- envOpts'
  pure $ bzipWith (<|>) envOpts'' (jsonOptsCustom configJson)

-- bzipWith :: (forall a. Maybe a -> Maybe a -> Maybe a) -> Options_ Maybe -> Options_ Maybe -> Options_ Maybe

instance (Alternative f) => Semigroup (Options_ f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (Options_ f) where
  mempty = buniq empty

getOptions' :: IO (Options_ Maybe)
getOptions' = envOpts' <> (jsonOptsCustom <$> readConfigFileDummy)

getOptions'' :: IO (Options_ Maybe)
getOptions'' = fold [envOpts', jsonOptsCustom <$> readConfigFileDummy]

withDefaults :: ProductB b => b Identity -> b Maybe -> b Identity
withDefaults = bzipWith fromMaybeI
  where
    fromMaybeI :: Identity a -> Maybe a -> Identity a
    fromMaybeI (Identity a) Nothing = Identity a
    fromMaybeI _ (Just a) = Identity a

type Options = Options_ Identity

-- defaultOpts :: Options_ Identity
-- defaultOpts =  Options_ {serverHost = "default-server", numThreads = 5, verbosity = 3}

getOptionsWithDefaults :: IO Options
getOptionsWithDefaults = withDefaults defaultOpts <$> fold [envOpts', jsonOptsCustom <$> readConfigFileDummy]

defaultOpts :: Options_ Identity
defaultOpts = error "not implemented"

optErrors :: Options_ (Const String)
optErrors =
  Options_
    { serverHost = Const "server host required but not provided",
      numThreads = "num threads required but not provided",
      verbosity = "verbosity required but not provided"
    }

validateOptions :: (TraversableB b, ProductB b) => b (Const String) -> b Maybe -> Validation (NonEmpty String) (b Identity)
validateOptions errMsgs mOpts = bsequence' $ bzipWith validate mOpts errMsgs
  where
    validate :: Maybe a -> Const String a -> Validation (NonEmpty String) a
    validate (Just x) _ = Success x
    validate Nothing (Const err) = Failure $ pure err

getOptions''' :: IO (Validation (NonEmpty String) Options)
getOptions''' =
  validateOptions optErrors <$> fold [envOpts', jsonOptsCustom <$> readConfigFileDummy]
