
module Text.Pandoc.Filter.Pyplot (
      makePlot
    ) where

import           Control.Monad   ((>=>))
import qualified Data.Map.Strict as M
import           System.FilePath (replaceExtension, isValid)

import           Text.Pandoc.Definition

import           Text.Pandoc.Filter.Scripting

data PandocPyplotError = ScriptError Int 
                       | InvalidTargetError FilePath

-- | Datatype containing all parameters required
-- to run pandoc-pyplot
data FigureSpec = FigureSpec 
    { target :: FilePath    -- ^ filepath where generated figure will be saved
    , alt :: String         -- ^ Alternate text for the figure (optional)
    , caption :: String     -- ^ Figure caption (optional)
    }

-- Keys that pandoc-pyplot will look for in code blocks
targetKey, altTextKey, captionKey :: String
targetKey  = "plot_target"
altTextKey = "plot_alt"
captionKey = "plot_caption"

-- | Determine inclusion specifications from Block attributes.
-- Note that the target key is required, but all other parameters are optional
parseFigureSpec :: M.Map String String -> Maybe FigureSpec
parseFigureSpec attrs = createInclusion <$> M.lookup targetKey attrs
    where
        defaultAltText = "Figure generated by pandoc-pyplot"
        defaultCaption = mempty
        createInclusion fname = FigureSpec
            { target  = fname
            , alt     = M.findWithDefault defaultAltText altTextKey attrs
            , caption = M.findWithDefault defaultCaption captionKey attrs
            }

-- | Format the script source based on figure spec.
formatScriptSource :: FigureSpec -> PythonScript -> PythonScript
formatScriptSource spec script = mconcat [ "# Source code for " <> target spec
                                         , "\n"
                                         , script 
                                         ]

-- | Main routine to include Matplotlib plots.
-- Code blocks containing the attributes @plot_target@ are considered
-- Python plotting scripts. All other possible blocks are ignored.
-- The source code is also saved in another file, which can be access by 
-- clicking the image
makePlot' :: Block -> IO (Either PandocPyplotError Block)
makePlot' cb @ (CodeBlock (id', cls, attrs) scriptSource) = 
    case parseFigureSpec (M.fromList attrs) of
        -- Could not parse - leave code block unchanged
        Nothing -> return $ Right cb
        -- Could parse : run the script and capture output
        Just spec -> do
            let figurePath = target spec
            
            -- Check that target filename is valid
            if not (isValid figurePath)
            then return $ Left $ InvalidTargetError figurePath
            else do
                script <- addPlotCapture figurePath scriptSource
                result <- runTempPythonScript script
                
                case result of
                    ScriptFailure code -> return $ Left $ ScriptError code
                    ScriptSuccess -> do 
                        -- Save the original script into a separate file
                        -- so it can be inspected
                        -- Note : using a .txt file allows to view source directly
                        --        in the browser, in the case of HTML output
                        let sourcePath = replaceExtension figurePath ".txt"
                        writeFile sourcePath $ formatScriptSource spec scriptSource
                        
                        -- Propagate attributes that are not related to pandoc-pyplot
                        let inclusionKeys = [ targetKey, altTextKey, captionKey ]
                            filteredAttrs = filter (\(k,_) -> k `notElem` inclusionKeys) attrs
                            image         = Image (id', cls, filteredAttrs) [Str $ alt spec] (figurePath, "")
                            srcTarget     = (sourcePath, "Click on this figure to see the source code")

                        -- TODO: use FigureSpec caption
                        -- We make the figure be a link to the source code
                        return $ Right $ Para [
                            Link nullAttr [image] srcTarget
                            ]
            
makePlot' x = return $ Right x

-- | Translate filter error to an error message
showError :: PandocPyplotError -> String
showError (ScriptError exitcode)     = "Script error: plot could not be generated. Exit code " <> (show exitcode)
showError (InvalidTargetError fname) = "Target filename " <> fname <> " is not valid."

-- | Highest-level function that can be walked over a Pandoc tree
makePlot :: Block -> IO Block
makePlot = makePlot' >=> either (fail . showError) return