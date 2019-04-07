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
      Configuration (..)
    , configuration
    -- * For testing and internal purposes only
    , inclusionKeys
    , directoryKey
    , captionKey
    , dpiKey
    , includePathKey
    , saveFormatKey
) where

import           Data.Maybe                    (fromMaybe)
import           Data.Default.Class            (Default, def)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Yaml         
import           Data.Yaml.Config              (loadYamlSettings, ignoreEnv)

import Text.Pandoc.Filter.Pyplot.FigureSpec
import Text.Pandoc.Filter.Pyplot.Scripting

-- | Keys that pandoc-pyplot will look for in code blocks. These are only exported for testing purposes.
directoryKey, captionKey, dpiKey, includePathKey, saveFormatKey :: String
directoryKey   = "directory"
captionKey     = "caption"
dpiKey         = "dpi"
includePathKey = "include"
saveFormatKey  = "format"

-- | list of all keys related to pandoc-pyplot.
inclusionKeys :: [String]
inclusionKeys = [ directoryKey
                , captionKey
                , dpiKey
                , includePathKey
                , saveFormatKey
                ]

-- | Configuration of pandoc-pyplot, describing the default behavior
-- of the filter. 
--
-- A Configuration is useful when dealing with lots of figures; it avoids
-- repeating the same values.sta
-- 
-- @since 2.1.0.0
data Configuration 
    = Configuration 
        { defaultDirectory     :: FilePath     -- ^ The default directory where figures will be saved.
        , defaultIncludeScript :: PythonScript -- ^ The default script to run before other instructions.
        , defaultSaveFormat    :: SaveFormat   -- ^ The default save format of generated figures.
        , defaultDPI           :: Int          -- ^ The default dots-per-inch value for generated figures.
        , interpreter          :: String       -- ^ The name of the interpreter to use to render figures.
        }
    deriving (Eq, Show)

instance Default Configuration where
    def = Configuration {
          defaultDirectory     = "generated/"
        , defaultIncludeScript = mempty
        , defaultSaveFormat    = PNG
        , defaultDPI           = 80
        , interpreter          = "python"
    }

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
        , defaultSaveFormat_  :: String
        , defaultDPI_         :: Int
        , interpreter_        :: String
        } 

instance FromJSON ConfigPrecursor where
    parseJSON (Object v) = ConfigPrecursor
        <$> v .:? (T.pack directoryKey)  .!= (defaultDirectory def)
        <*> v .:? (T.pack includePathKey)
        <*> v .:? (T.pack saveFormatKey) .!= (extension $ defaultSaveFormat def)
        <*> v .:? (T.pack dpiKey)        .!= (defaultDPI def)
        <*> v .:? "interpreter"          .!= (interpreter def)
    
    parseJSON _ = fail "Could not parse the configuration"

renderConfiguration :: ConfigPrecursor -> IO Configuration
renderConfiguration prec = do
    includeScript <- fromMaybe mempty $ T.readFile <$> defaultIncludePath_ prec
    let saveFormat' = fromMaybe (defaultSaveFormat def) $ saveFormatFromString $ defaultSaveFormat_ prec
    return $ Configuration { defaultDirectory     = defaultDirectory_ prec
                           , defaultIncludeScript = includeScript
                           , defaultSaveFormat    = saveFormat'
                           , defaultDPI           = defaultDPI_ prec
                           , interpreter          = interpreter_ prec
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