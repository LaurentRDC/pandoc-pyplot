{-|
Module      : Text.Pandoc.Filter.Pyplot.Cleanup
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines functions to clean-up the pandoc-pyplot generated outputs
by walking over a document and gathering all figure directories.
-}

module Text.Pandoc.Pyplot.Filter.Cleanup where

import           Control.Monad                 (forM_)     
import qualified Data.Map.Strict               as Map

import           System.FilePath               (makeValid)
import           System.Directory              ( doesDirectoryExist
                                               , removeDirectoryRecursive
                                               )

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk              (query)

import           Text.Pandoc.Filter.Pyplot.Configuration (directoryKey)
import           Text.Pandoc.Filter.Pyplot.Types

type Directory = FilePath

-- | Get the target directory of every Pandoc block that could
-- become a pandoc-pyplot figure
figureSpecDirectory :: Configuration -> Block -> [Directory]
figureSpecDirectory config (CodeBlock (id', cls, attrs) content)
    | "pyplot" `elem` cls = [dir]
    | otherwise = []
    where
        attrs' = Map.fromList attrs
        dir = makeValid $ Map.findWithDefault (defaultDirectory config) directoryKey attrs'
figureSpecDirectory _ _ = []

-- | Extract all figure directories from a Pandoc document
figureDirectories :: Configuration -> Pandoc -> [Directory]
figureDirectories config = query (figureSpecDirectory config)

-- | Cleanup directories in 
cleanup :: [Directory] -> IO ()
cleanup dirs = forM_ dirs  
    (\d -> do     
        exists <- doesDirectoryExist d
        if exists
            then removeDirectoryRecursive d
            else return ()
        )