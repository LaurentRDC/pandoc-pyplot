{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}

{-|
Module      : Text.Pandoc.Filter.Pyplot
Description : Pandoc filter to create Matplotlib figures from code blocks
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable

This module defines a Pandoc filter @makePlot@ that can be
used to walk over a Pandoc document and generate figures from
Python code blocks.

The syntax for code blocks is simple, Code blocks with the @.pyplot@
attribute will trigger the filter. The code block will be reworked into a Python
script and the output figure will be captured, along with a high-resolution version
of the figure and the source code used to generate the figure.

To trigger pandoc-pyplot, the following is __required__:

    * @.pyplot@: Trigger pandoc-pyplot but let it decide on a filename

Here are the possible attributes what pandoc-pyplot understands:

    * @target=...@: Filepath where the resulting figure should be saved.
    * @directory=...@ : Directory where to save the figure.
    * @caption="..."@: Specify a plot caption (or alternate text).
    * @dpi=...@: Specify a value for figure resolution, or dots-per-inch. Default is 80DPI.
    * @include=...@: Path to a Python script to include before the code block. Ideal to avoid repetition over many figures.

Here are some example blocks in Markdown:

@
This is a paragraph

```{.pyplot caption="This is a caption."}
import matplotlib.pyplot as plt

plt.figure()
plt.plot([0,1,2,3,4], [1,2,3,4,5])
plt.title('This is an example figure')
```
@

This filter was originally designed to be used with [Hakyll](https://jaspervdj.be/hakyll/). 
In case you want to use the filter with your own Hakyll setup, you can use a transform 
function that works on entire documents:

@
import Text.Pandoc.Filter.Pyplot (plotTransform)

import Hakyll

-- Unsafe compiler is required because of the interaction
-- in IO (i.e. running an external Python script).
makePlotPandocCompiler :: Compiler (Item String)
makePlotPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (unsafeCompiler . plotTransform)
@

-}
module Text.Pandoc.Filter.Pyplot
    ( makePlot
    , plotTransform
    , PandocPyplotError(..)
      -- For testing purposes only
    , makePlot'
    , directoryKey
    , captionKey
    , dpiKey
    , includePathKey
    , saveFormatKey
    ) where

import           Control.Monad                 ((>=>))

import           Data.List                     (intersperse)

import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Version                  (showVersion)

import           Paths_pandoc_pyplot           (version)

import           System.Directory              (createDirectoryIfMissing,
                                                doesFileExist)
import           System.FilePath               (makeValid, takeDirectory)

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk              (walkM)

import           Text.Pandoc.Filter.FigureSpec (FigureSpec (..),
                                                SaveFormat (..), addPlotCapture,
                                                figurePath, sourceCodePath, saveFormatFromString, 
                                                toImage)
import           Text.Pandoc.Filter.Scripting

-- | Possible errors returned by the filter
data PandocPyplotError
    = ScriptError Int    -- ^ Running Python script has yielded an error
    | BlockingCallError  -- ^ Python script contains a block call to 'show()'
    deriving (Eq)

instance Show PandocPyplotError where
    show (ScriptError exitcode) = "Script error: plot could not be generated. Exit code " <> (show exitcode)
    show BlockingCallError      = "Script contains a blocking call to show, like 'plt.show()'"

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

-- | Determine inclusion specifications from Block attributes.
-- Note that the @".pyplot"@ class is required, but all other parameters are optional
parseFigureSpec :: Block -> IO (Maybe FigureSpec)
parseFigureSpec (CodeBlock (id', cls, attrs) content)
    | "pyplot" `elem` cls = Just <$> figureSpec
    | otherwise = return Nothing
  where
    attrs'        = Map.fromList attrs
    filteredAttrs = filter (\(k, _) -> k `notElem` inclusionKeys) attrs
    dir           = makeValid $ Map.findWithDefault "generated" directoryKey attrs'
    format        = fromMaybe (PNG) $ saveFormatFromString $ Map.findWithDefault "png" saveFormatKey attrs'
    includePath   = Map.lookup includePathKey attrs'

    figureSpec :: IO FigureSpec
    figureSpec = do
        includeScript <- fromMaybe (return mempty) $ T.readFile <$> includePath
        let header      = "# Generated by pandoc-pyplot " <> ((T.pack . showVersion) version)
            fullScript  = mconcat $ intersperse "\n" [header, includeScript, T.pack content]
            caption'    = Map.findWithDefault mempty captionKey attrs'
            dpi'        = read $ Map.findWithDefault "80" dpiKey attrs'
            blockAttrs' = (id', filter (/= "pyplot") cls, filteredAttrs)
        return $ FigureSpec caption' fullScript format dir dpi' blockAttrs'

parseFigureSpec _ = return Nothing

-- | Run the Python script. In case the file already exists, we can safely assume
-- there is no need to re-run it.
runScriptIfNecessary :: FigureSpec -> IO ScriptResult
runScriptIfNecessary spec = do
    createDirectoryIfMissing True . takeDirectory $ figurePath spec
    fileAlreadyExists <- doesFileExist $ figurePath spec
    result <- if fileAlreadyExists
                then return ScriptSuccess
                else runTempPythonScript $ addPlotCapture spec
    case result of
        ScriptFailure code -> return $ ScriptFailure code
        ScriptSuccess      -> T.writeFile (sourceCodePath spec) (script spec) >> return ScriptSuccess

-- | Main routine to include Matplotlib plots.
-- Code blocks containing the attributes @.pyplot@ are considered
-- Python plotting scripts. All other possible blocks are ignored.
makePlot' :: Block -> IO (Either PandocPyplotError Block)
makePlot' block = do
    parsed <- parseFigureSpec block  
    case parsed of
        Nothing   -> return $ Right block
        Just spec ->
            if hasBlockingShowCall (script spec)
                then return $ Left BlockingCallError
                else handleResult spec <$> runScriptIfNecessary spec 
    where
        handleResult _   (ScriptFailure code) = Left  $ ScriptError code
        handleResult spec ScriptSuccess       = Right $ toImage spec

-- | Highest-level function that can be walked over a Pandoc tree.
-- All code blocks that have the '.pyplot' parameter will be considered
-- figures.
makePlot :: Block -> IO Block
makePlot = makePlot' >=> either (fail . show) return

-- | Walk over an entire Pandoc document, changing appropriate code blocks
-- into figures.
plotTransform :: Pandoc -> IO Pandoc
plotTransform = walkM makePlot
