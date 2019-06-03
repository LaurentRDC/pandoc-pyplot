{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Filter.Pyplot.Configuration
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Configuration for pandoc-pyplot
-}

module Text.Pandoc.Filter.Pyplot.Configuration (
      configuration
    -- * For testing and internal purposes only
    , writeConfig
    , inclusionKeys
    , directoryKey
    , captionKey
    , dpiKey
    , includePathKey
    , saveFormatKey
    , withLinksKey
) where

import           Data.Maybe                    (fromMaybe)
import           Data.Default.Class            (def)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Yaml         
import           Data.Yaml.Config              (loadYamlSettings, ignoreEnv)

import           System.Directory              (doesFileExist)

import Text.Pandoc.Filter.Pyplot.Types

-- | Keys that pandoc-pyplot will look for in code blocks. These are only exported for testing purposes.
directoryKey, captionKey, dpiKey, includePathKey, saveFormatKey, withLinksKey :: String
directoryKey   = "directory"
captionKey     = "caption"
dpiKey         = "dpi"
includePathKey = "include"
saveFormatKey  = "format"
withLinksKey   = "with-links"

-- | list of all keys related to pandoc-pyplot.
inclusionKeys :: [String]
inclusionKeys = [ directoryKey
                , captionKey
                , dpiKey
                , includePathKey
                , saveFormatKey
                , withLinksKey
                ]

-- A @Configuration@ cannot be directly created from a YAML file
-- for two reasons:
--
--     * we want to store an include script. However, it makes more sense to 
--       specify the script path in a YAML file.
--     * Save format is best specified by a string, and this must be parsed later 
--
-- Therefore, we have another type, ConfigPrecursor, which CAN be created directly from
-- a YAML file.
data ConfigPrecursor
    = ConfigPrecursor
        { defaultDirectory_   :: FilePath
        , defaultIncludePath_ :: Maybe FilePath
        , defaultWithLinks_   :: Bool
        , defaultSaveFormat_  :: String
        , defaultDPI_         :: Int
        , interpreter_        :: String
        , flags_              :: [String]
        } 

instance FromJSON ConfigPrecursor where
    parseJSON (Object v) = ConfigPrecursor
        <$> v .:? (T.pack directoryKey)  .!= (defaultDirectory def)
        <*> v .:? (T.pack includePathKey)
        <*> v .:? (T.pack withLinksKey)  .!= (defaultWithLinks def)
        <*> v .:? (T.pack saveFormatKey) .!= (extension $ defaultSaveFormat def)
        <*> v .:? (T.pack dpiKey)        .!= (defaultDPI def)
        <*> v .:? "interpreter"          .!= (interpreter def)
        <*> v .:? "flags"                .!= (flags def)
    
    parseJSON _ = fail "Could not parse the configuration"

renderConfiguration :: ConfigPrecursor -> IO Configuration
renderConfiguration prec = do
    includeScript <- fromMaybe mempty $ T.readFile <$> defaultIncludePath_ prec
    let saveFormat' = fromMaybe (defaultSaveFormat def) $ saveFormatFromString $ defaultSaveFormat_ prec
    return $ Configuration { defaultDirectory     = defaultDirectory_ prec
                           , defaultIncludeScript = includeScript
                           , defaultSaveFormat    = saveFormat'
                           , defaultWithLinks     = defaultWithLinks_ prec
                           , defaultDPI           = defaultDPI_ prec
                           , interpreter          = interpreter_ prec
                           , flags                = flags_ prec
                           }


-- | Building configuration from a YAML file. The
-- keys are exactly the same as for Markdown code blocks.
--
-- If a key is either not present or unreadable, its value will be set
-- to the default value.
--
-- @since 2.1.0.0
configuration :: FilePath -> IO Configuration
configuration fp = loadYamlSettings [fp] [] ignoreEnv >>= renderConfiguration


-- | Write a configuration to file. An exception will be raised in case the file would be overwritten.
--
-- @since 2.1.3.0
writeConfig :: FilePath -> Configuration -> IO ()
writeConfig fp config = do
    fileExists <- doesFileExist fp
    if fileExists
        then error $ mconcat ["File ", fp, " already exists."]
        else encodeFile fp config