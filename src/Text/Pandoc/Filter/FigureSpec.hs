{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Filter.Figure
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types and functions that help
with running Python scripts.
-}

module Text.Pandoc.Filter.FigureSpec (
        FigureSpec (..)
      , figurePath
) where

import           Data.Hashable                (Hashable, hashWithSalt, hash)

import           System.FilePath              (FilePath, addExtension, (</>))

import           Text.Pandoc.Filter.Scripting
import           Text.Pandoc.Definition       (Attr)

-- | Datatype containing all parameters required
-- to run pandoc-pyplot
data FigureSpec = FigureSpec
    { caption     :: String          -- ^ Figure caption.
    , script      :: PythonScript    -- ^ Source code for the figure.
    , directory   :: FilePath        -- ^ Directory where to save the file
    , dpi         :: Int             -- ^ Dots-per-inch of figure
    , blockAttrs  :: Attr            -- ^ Attributes not related to @pandoc-pyplot@ will be propagated.
    }

instance Hashable FigureSpec where
    hashWithSalt salt spec = 
        hashWithSalt salt (caption spec, script spec, directory spec, dpi spec, blockAttrs spec)

-- | Determine the path a figure should have.
figurePath :: FigureSpec -> FilePath
figurePath spec =  (directory spec </> stem spec)
    where stem = flip addExtension ".png" . show . hash