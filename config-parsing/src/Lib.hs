{-# LANGUAGE DerivingStrategies #-}

module Lib
  ( someFunc,
  )
where

import Data.List.NonEmpty
import Data.Version
import Debug.Trace
import OptEnvConf
import System.Directory (getHomeDirectory)

someFunc :: IO ()
someFunc = do
  let version = makeVersion [0] -- what if my version is a string?
  c <- runSettingsParser version
  print (c :: Config)

data Config = Config
  { configInt :: Int,
    configString :: String,
    configList :: [String],
    configCommand :: Command
  }
  deriving stock (Eq, Show)

homedirParser :: Parser FilePath
homedirParser = mapIO (const getHomeDirectory) (pure ())

instance HasParser Config where
  settingsParser =
    -- 1. is that a correct way to assemble a parser that reads from the config file in the home directory?
    -- 2. it seems that only one of the files is taken into account when looking for options
    withYamlConfig ((\home -> Just $ home <> "/config.yaml") <$> homedirParser) $
      withLocalYamlConfig $
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
