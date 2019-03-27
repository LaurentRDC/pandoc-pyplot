{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Text.Pandoc.Filter.Figure
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types and functions that help
with keeping track of figure specifications
-}
module Text.Pandoc.Filter.FigureSpec
    ( FigureSpec(..)
    , figurePath
    , hiresFigurePath
    , addPlotCapture
    ) where

import           Data.Hashable                (Hashable, hash, hashWithSalt)
import qualified Data.Text                    as T

import           System.FilePath              (FilePath, addExtension,
                                               replaceExtension, (</>))

import           Text.Pandoc.Definition       (Attr)
import           Text.Pandoc.Filter.Scripting (PythonScript)

data SaveFormat
    = PNG
    | PDF
    | SVG
    | JPG

-- | Datatype containing all parameters required
-- to run pandoc-pyplot
data FigureSpec = FigureSpec
    { caption    :: String -- ^ Figure caption.
    , script     :: PythonScript -- ^ Source code for the figure.
    , directory  :: FilePath -- ^ Directory where to save the file
    , dpi        :: Int -- ^ Dots-per-inch of figure
    , blockAttrs :: Attr -- ^ Attributes not related to @pandoc-pyplot@ will be propagated.
    }

instance Hashable FigureSpec where
    hashWithSalt salt spec =
        hashWithSalt salt (caption spec, script spec, directory spec, dpi spec, blockAttrs spec)

-- | Determine the path a figure should have.
figurePath :: FigureSpec -> FilePath
figurePath spec = (directory spec </> stem spec)
  where
    stem = flip addExtension ".png" . show . hash

-- | The path to the high-resolution figure.
hiresFigurePath :: FigureSpec -> FilePath
hiresFigurePath = flip replaceExtension ".hires.png" . figurePath

-- | Modify a Python plotting script to save the figure to a filename.
-- An additional file (with extension PNG) will also be captured.
addPlotCapture ::
       FigureSpec -- ^ Path where to save the figure
    -> PythonScript -- ^ Code block with added capture
addPlotCapture spec =
    mconcat
        [ script spec
        , "\nimport matplotlib.pyplot as plt" -- Just in case
        , plotCapture (figurePath spec) (dpi spec)
        , plotCapture (hiresFigurePath spec) (minimum [200, 2 * dpi spec])
        ]
  where
    plotCapture fname' dpi' =
        mconcat
            [ "\nplt.savefig("
            , T.pack $ show fname' -- show is required for quotes
            , ", dpi="
            , T.pack $ show dpi'
            , ")"
            ]