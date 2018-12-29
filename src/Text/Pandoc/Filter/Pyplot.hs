{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Unsafe     #-}
{-|
Module      : Text.Pandoc.Filter.Pyplot
Description : Pandoc filter to create Matplotlib figures from code blocks
Copyright   : (c) Laurent P René de Cotret, 2018
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable

This module defines a Pandoc filter @makePlot@ that can be 
used to walk over a Pandoc document and generate figures from
Python code blocks.

The syntax for code blocks is simple, Code blocks with the @plot_target=...@
attribute will trigger the filter. The code block will be reworked into a Python
script and the output figure will be captured.

Here are the possible attributes what pandoc-pyplot understands:

    * @plot_target=...@ (_required_): Filepath where the resulting figure should be saved.
    * @plot_alt="..."@ (_optional_): Specify a plot caption (or alternate text).
    * @plot_include=...@ (_optional_): Path to a Python script to include before the code block. 
    Ideal to avoid repetition over many figures.

Here are some example blocks in Markdown:

@
This is a paragraph

```{plot_target=my_figure.jpg plot_alt="This is a caption."}
import matplotlib.pyplot as plt

plt.figure()
plt.plot([0,1,2,3,4], [1,2,3,4,5])
plt.title('This is an example figure')
```
@
-}
module Text.Pandoc.Filter.Pyplot (
        makePlot
      , makePlot' -- For testing
      , plotTransform
      , PandocPyplotError(..)
    ) where

import           Control.Monad                  ((>=>))
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import           System.Directory               (doesDirectoryExist)
import           System.FilePath                (isValid, replaceExtension, takeDirectory)

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk               (walkM)

import           Text.Pandoc.Filter.Scripting   

-- | Possible errors returned by the filter
data PandocPyplotError = ScriptError Int                -- ^ Running Python script has yielded an error
                       | InvalidTargetError FilePath    -- ^ Invalid figure path
                       | MissingDirectoryError FilePath -- ^ Directory where to save figure does not exist
                       | BlockingCallError              -- ^ Python script contains a block call to 'show()'
        deriving Eq

instance Show PandocPyplotError where
    -- | Translate filter error to an error message
    show (ScriptError exitcode)          = "Script error: plot could not be generated. Exit code " <> (show exitcode)
    show (InvalidTargetError fname)      = "Target filename " <> fname <> " is not valid."
    show (MissingDirectoryError dirname) = "Target directory " <> dirname <> " does not exist." 
    show BlockingCallError               = "Script contains a blocking call to show, like 'plt.show()'"


-- | Datatype containing all parameters required
-- to run pandoc-pyplot
data FigureSpec = FigureSpec 
    { target      :: FilePath       -- ^ filepath where generated figure will be saved.
    , alt         :: String         -- ^ Alternate text for the figure (optional).
    , script      :: PythonScript   -- ^ Source code for the figure.
    , includePath :: Maybe FilePath -- ^ Path to a Python to be included before the script.
    , blockAttrs  :: Attr           -- ^ Attributes not related to @pandoc-pyplot@ will be propagated.
    }

-- | Use figure specification to render a full plot script, including everything except plot capture
renderScript :: FigureSpec -> IO PythonScript
renderScript spec = do
    includeScript <- fromMaybe (return "") $ readFile <$> (includePath spec)
    return $ mconcat ["# Source code for ", target spec, "\n", includeScript, "\n", script spec]

-- Keys that pandoc-pyplot will look for in code blocks
targetKey, altTextKey, includePathKey :: String
targetKey      = "plot_target"
altTextKey     = "plot_alt"
includePathKey = "plot_include"

-- | Determine inclusion specifications from Block attributes.
-- Note that the target key is required, but all other parameters are optional
parseFigureSpec :: Block -> Maybe FigureSpec
parseFigureSpec (CodeBlock (id', cls, attrs) content) = 
    createInclusion <$> M.lookup targetKey attrs'
    where
        attrs' = M.fromList attrs
        inclusionKeys = [ targetKey, altTextKey ]
        filteredAttrs = filter (\(k,_) -> k `notElem` inclusionKeys) attrs
        createInclusion fname = FigureSpec
            { target     = fname
            , alt        = M.findWithDefault "Figure generated by pandoc-pyplot" altTextKey attrs'
            , script     = content
            , includePath = M.lookup includePathKey attrs'
            -- Propagate attributes that are not related to pandoc-pyplot
            , blockAttrs = (id', cls, filteredAttrs)
            }
parseFigureSpec _ = Nothing

-- | Main routine to include Matplotlib plots.
-- Code blocks containing the attributes @plot_target@ are considered
-- Python plotting scripts. All other possible blocks are ignored.
-- The source code is also saved in another file, which can be access by 
-- clicking the image
makePlot' :: Block -> IO (Either PandocPyplotError Block)
makePlot' block = 
    case parseFigureSpec block of
        -- Could not parse - leave code block unchanged
        Nothing -> return $ Right block
        -- Could parse : run the script and capture output
        Just spec -> do
            
            -- Rendered script, including possible inclusions and other additions
            -- except the plot capture.
            rendered <- renderScript spec

            let figurePath = target spec
                figureDir = takeDirectory figurePath
            
            -- Check that the directory in which to save the figure exists
            validDirectory <- doesDirectoryExist $ takeDirectory figurePath
            
            if | not (isValid figurePath)         -> return $ Left $ InvalidTargetError figurePath
               | not validDirectory               -> return $ Left $ MissingDirectoryError figureDir
               | hasBlockingShowCall rendered     -> return $ Left $ BlockingCallError
               | otherwise -> do 
                    
                -- Running the script
                -- A plot capture (plt.savefig(...)) is added as well
                result <- runTempPythonScript $ addPlotCapture (target spec) rendered
                
                case result of
                    ScriptFailure code -> return $ Left $ ScriptError code
                    ScriptSuccess -> do 
                        -- Save the original script into a separate file
                        -- so it can be inspected
                        -- Note : using a .txt file allows to view source directly
                        --        in the browser, in the case of HTML output
                        let sourcePath = replaceExtension figurePath ".txt"
                        writeFile sourcePath rendered
                        
                        -- Propagate attributes that are not related to pandoc-pyplot
                        let relevantAttrs = blockAttrs spec
                            srcTarget = Link nullAttr [Str "Source code"] (sourcePath, "")
                            caption'   = [Str $ alt spec, Space, Str "(", srcTarget, Str ")"]
                            -- To render images as figures with captions, the target title
                            -- must be "fig:"
                            -- Janky? yes
                            image     = Image relevantAttrs caption' (figurePath, "fig:")

                        return $ Right $ Para $ [image]

-- | Highest-level function that can be walked over a Pandoc tree.
-- All code blocks that have the 'plot_target' parameter will be considered
-- figures.
makePlot :: Block -> IO Block
makePlot = makePlot' >=> either (fail . show) return

-- | Walk over an entire Pandoc document, changing appropriate code blocks
-- into figures.
plotTransform :: Pandoc -> IO Pandoc
plotTransform = walkM makePlot
