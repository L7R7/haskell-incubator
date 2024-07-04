{-# LANGUAGE DerivingStrategies #-}

module Lib
  ( someFunc,
    Config (..),
  )
where

import Autodocodec.Yaml
import Control.Monad
import Data.Aeson
import Data.Aeson.KeyMap (unionWith)
import Data.List.NonEmpty
import Data.Version
import OptEnvConf
import Path
import Path.IO

someFunc :: IO ()
someFunc = do
  let version = makeVersion [0]
  c <- runSettingsParser version
  print (c :: Config)

data Config = Config
  { configInt :: Int,
    configString :: String,
    configList :: [String],
    configCommand :: Command
  }
  deriving stock (Eq, Show)

homeParser :: Parser (Path Abs Dir)
homeParser = mapIO id (pure getHomeDir)

homeDirConfig :: Parser FilePath
homeDirConfig = mapIO (\home -> toFilePath <$> resolveFile home "config.yaml") homeParser

localConfig :: Parser FilePath
localConfig = mapIO id (pure (toFilePath <$> resolveFile' "config.yaml"))

combinedConfigs :: Parser (NonEmpty FilePath)
combinedConfigs = sequenceA $ localConfig :| [homeDirConfig]

instance HasParser Config where
  settingsParser =
    subConfig "sup" $
      withYamlConfigs combinedConfigs $
        Config
          <$> setting
            [ help "number of something",
              reader auto,
              long "int",
              short 'i',
              option,
              env "FOO_INT",
              conf "config-int",
              metavar "INT"
            ]
          <*> setting
            [ help "this is a string",
              reader str,
              long "string",
              short 's',
              option,
              env "FOO_STRING",
              conf "config-string",
              metavar "STR"
            ]
          <*> setting
            [ help "list of somethings",
              reader (toList <$> commaSeparated str),
              long "list",
              short 'l',
              option,
              env "FOO_LIST",
              conf "config-list",
              metavar "LIST"
            ]
          <*> settingsParser

data Execution = DryRun | Execute deriving stock (Eq, Show)

-- is this how it's done? Or should this be a HasParser instance? Where is the difference?
executionParser :: Parser Execution
executionParser =
  setting
    [ help "Control whether to actually execute things or perform a dry run",
      switch Execute,
      long "execute",
      short 'x',
      value DryRun
    ]

data Command
  = Foo
  | Bar Int
  | Baz Execution
  | Qux SubCommand Execution
  deriving stock (Eq, Show)

instance HasParser Command where
  settingsParser =
    commands
      [ command "foo" "Foo" $ pure Foo,
        command "bar" "Bar" $ Bar <$> setting [help "bar things", reader auto, argument, metavar "INT"],
        command "baz" "Baz" $ Baz <$> executionParser,
        command "qux" "Qux" $ Qux <$> settingsParser <*> executionParser
      ]

data SubCommand = SubOne Int | SubTwo String
  deriving stock (Eq, Show)

instance HasParser SubCommand where
  settingsParser =
    commands
      [ command "one" "first subcommand" $ SubOne <$> setting [help "severity level", reader auto, argument, metavar "SEVERITY"],
        command "two" "second subcommand" $ SubTwo <$> setting [help "lorem ipsum", reader str, argument, metavar "TXT"]
      ]

withYamlConfigs :: Parser (NonEmpty FilePath) -> Parser a -> Parser a
withYamlConfigs parsers = withConfig $ mapIO (foldM resolveYamlConfigFile Nothing) parsers
  where
    resolveYamlConfigFile :: Maybe Object -> FilePath -> IO (Maybe Object)
    resolveYamlConfigFile acc = fmap (combineMaybeObjects acc . join) . (resolveFile' >=> readYamlConfigFile)
    -- left biased, first one wins
    combineMaybeObjects :: Maybe Object -> Maybe Object -> Maybe Object
    combineMaybeObjects Nothing mo = mo
    combineMaybeObjects mo Nothing = mo
    combineMaybeObjects (Just o1) (Just o2) = Just (combineObjects o1 o2)
      where
        combineObjects :: Object -> Object -> Object
        combineObjects = unionWith combineValues
        combineValues :: Value -> Value -> Value
        combineValues (Object o) (Object o') = Object (combineObjects o o')
        combineValues v _ = v
