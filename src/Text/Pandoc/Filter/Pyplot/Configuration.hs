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

import           Prelude                       hiding (lookup)

import           Control.Monad                 (join)

import           Data.Maybe                    (fromMaybe)
import           Data.Default.Class            (Default, def)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Yaml.Config              (lookupDefault, lookup, load)

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
-- repeating the same values.
-- 
-- @since 2.1.0.0
data Configuration 
    = Configuration 
        { defaultDirectory     :: FilePath
        , defaultIncludeScript :: PythonScript
        , defaultSaveFormat    :: SaveFormat
        , defaultDPI           :: Int
        }

instance Default Configuration where
    def = Configuration {
          defaultDirectory     = "generated/"
        , defaultIncludeScript = mempty
        , defaultSaveFormat    = PNG
        , defaultDPI           = 80
    }

-- | Building configuration from a YAML file. The
-- keys are exactly the same as for Markdown code blocks.
--
-- @since 2.1.0.0
configuration :: FilePath -> IO Configuration
configuration fp = do
    c <- load fp
    inc <- fromMaybe (return $ defaultIncludeScript def) $ T.readFile <$> lookup (T.pack includePathKey) c
    
    let dir = lookupDefault (T.pack directoryKey) (defaultDirectory def) c
        fmt = fromMaybe (defaultSaveFormat def) $ join $ saveFormatFromString <$> lookup (T.pack saveFormatKey) c
        dpi' = lookupDefault (T.pack dpiKey) (defaultDPI def) c
    
    
    return $ Configuration  dir inc fmt dpi'