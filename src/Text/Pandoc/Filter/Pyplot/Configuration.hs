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
    -- For testing purposes only
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
inclusionKeys = [directoryKey, captionKey, dpiKey, includePathKey, saveFormatKey]


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

configuration :: FilePath -> IO Configuration
configuration fp = do
    c <- load fp
    inc <- fromMaybe (return $ defaultIncludeScript def) $ T.readFile <$> lookup (T.pack includePathKey) c
    
    let dir = lookupDefault (T.pack directoryKey) (defaultDirectory def) c
        fmt = fromMaybe (defaultSaveFormat def) $ join $ saveFormatFromString <$> lookup (T.pack saveFormatKey) c
        dpi' = lookupDefault (T.pack dpiKey) (defaultDPI def) c
    
    
    return $ Configuration  dir inc fmt dpi'