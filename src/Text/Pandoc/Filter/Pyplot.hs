{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

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

    * @directory=...@ : Directory where to save the figure.
    * @format=...@: Format of the generated figure. This can be an extension or an acronym, e.g. @format=png@.
    * @caption="..."@: Specify a plot caption (or alternate text). Captions support Markdown formatting and LaTeX math (@$...$@).
    * @dpi=...@: Specify a value for figure resolution, or dots-per-inch. Default is 80DPI.
    * @include=...@: Path to a Python script to include before the code block. Ideal to avoid repetition over many figures.
    * @links=true|false@: Add links to source code and high-resolution version of this figure. 
      This is @true@ by default, but you may wish to disable this for PDF output.
      
Here are some example blocks in Markdown:

@
This is a paragraph

```{.pyplot caption="This is a caption."}
import matplotlib.pyplot as plt

plt.figure()
plt.plot([0,1,2,3,4], [1,2,3,4,5])
plt.title('This is an example figure')
```

This is another paragraph

```{.pyplot dpi=150 format=SVG}
# This example was taken from the Matplotlib gallery
# https://matplotlib.org/examples/pylab_examples/bar_stacked.html

import numpy as np
import matplotlib.pyplot as plt

N = 5
menMeans = (20, 35, 30, 35, 27)
womenMeans = (25, 32, 34, 20, 25)
menStd = (2, 3, 4, 1, 2)
womenStd = (3, 5, 2, 3, 3)
ind = np.arange(N)    # the x locations for the groups
width = 0.35       # the width of the bars: can also be len(x) sequence

p1 = plt.bar(ind, menMeans, width, color='#d62728', yerr=menStd)
p2 = plt.bar(ind, womenMeans, width,
             bottom=menMeans, yerr=womenStd)

plt.ylabel('Scores')
plt.title('Scores by group and gender')
plt.xticks(ind, ('G1', 'G2', 'G3', 'G4', 'G5'))
plt.yticks(np.arange(0, 81, 10))
plt.legend((p1[0], p2[0]), ('Men', 'Women'))
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

Custom configurations are possible via the @Configuration@ type and the filter 
functions @plotTransformWithConfig@ and @makePlotWithConfig@. 
-}
module Text.Pandoc.Filter.Pyplot (
    -- * Operating on single Pandoc blocks 
      makePlot
    , makePlotWithConfig
    -- * Operating on whole Pandoc documents
    , plotTransform
    , plotTransformWithConfig
    -- * For configuration purposes
    , configuration
    , Configuration (..)
    , PythonScript
    , SaveFormat (..)
    -- * For testing and internal purposes only
    , PandocPyplotError(..)
    , makePlot'
    ) where

import           Control.Monad                 ((>=>))

import           Data.Default.Class            (def)

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk              (walkM)

import           Text.Pandoc.Filter.Pyplot.Internal

-- | Main routine to include Matplotlib plots.
-- Code blocks containing the attributes @.pyplot@ are considered
-- Python plotting scripts. All other possible blocks are ignored.
makePlot' :: Configuration -> Block -> IO (Either PandocPyplotError Block)
makePlot' config block = do
    parsed <- parseFigureSpec config block  
    case parsed of
        Nothing   -> return $ Right block
        Just spec -> handleResult spec <$> runScriptIfNecessary config spec

    where
        handleResult _ (ScriptChecksFailed msg) = Left  $ ScriptChecksFailedError msg
        handleResult _ (ScriptFailure code)     = Left  $ ScriptError code
        handleResult spec ScriptSuccess         = Right $ toImage spec

-- | Highest-level function that can be walked over a Pandoc tree.
-- All code blocks that have the '.pyplot' parameter will be considered
-- figures.
makePlot :: Block -> IO Block
makePlot = makePlotWithConfig def

-- | like @makePlot@ with with a custom default values.
--
-- @since 2.1.0.0
makePlotWithConfig :: Configuration -> Block -> IO Block
makePlotWithConfig config = makePlot' config >=> either (fail . show) return

-- | Walk over an entire Pandoc document, changing appropriate code blocks
-- into figures. Default configuration is used.
plotTransform :: Pandoc -> IO Pandoc
plotTransform = walkM makePlot

-- | Walk over an entire Pandoc document, changing appropriate code blocks
-- into figures. The default values are determined by a @Configuration@.
--
-- @since 2.1.0.0
plotTransformWithConfig :: Configuration -> Pandoc -> IO Pandoc
plotTransformWithConfig = walkM . makePlotWithConfig
