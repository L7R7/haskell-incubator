{-# LANGUAGE DerivingStrategies #-}

module Lib
  ( someFunc,
    Config (..),
  )
where

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
homeParser = mapIO (const getHomeDir) (pure ())

-- is that a correct way to assemble a parser that reads from the config file in the home directory?
withHomeDirYamlConfig :: Parser a -> Parser a
withHomeDirYamlConfig = withYamlConfig $ mapIO (\home -> Just . toFilePath <$> resolveFile home "config.yaml") homeParser

instance HasParser Config where
  settingsParser =
    -- it seems that only one of the files is taken into account when looking for options, am I missing something?
    withHomeDirYamlConfig $
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

{-
 * if I call it with qux --help, the whole help is printed. Maybe it makes sense to only print the help text for the subcommand? Especially for more complicated, nested Parsers
 * if more than one setting is missing, only the first is reported. Is it possible to report all of them at once?
-}
