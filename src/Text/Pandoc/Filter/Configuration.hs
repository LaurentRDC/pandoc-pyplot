{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Filter.Configuration
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Configuration for pandoc-pyplot
-}

module Text.Pandoc.Filter.Configuration (
      Configuration (..)
    , buildConfiguration
) where

import           Prelude                       hiding (lookup)

import           Control.Monad                 (join)

import           Data.Maybe                    (fromMaybe)
import           Data.Default.Class            (Default, def)
import qualified Data.Text.IO                  as T
import           Data.Yaml.Config              (lookupDefault, lookup, load)

import           System.Directory              (doesFileExist)

import           Text.Pandoc.Filter.FigureSpec (SaveFormat(..), saveFormatFromString)
import           Text.Pandoc.Filter.Scripting  (PythonScript)

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

buildConfiguration :: FilePath -> IO Configuration
buildConfiguration fp = do
    c <- load fp
    inc <- fromMaybe (return $ defaultIncludeScript def) $ T.readFile <$> lookup "include" c
    
    let dir = lookupDefault "directory" (defaultDirectory def) c
        fmt = fromMaybe (defaultSaveFormat def) $ join $ saveFormatFromString <$> lookup "format" c
        dpi = lookupDefault "dpi" (defaultDPI def) c
    
    
    return $ Configuration  dir inc fmt dpi