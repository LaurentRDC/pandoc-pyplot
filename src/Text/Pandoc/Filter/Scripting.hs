
module Text.Pandoc.Filter.Scripting (
      runTempPythonScript
    , addPlotCapture
    , PythonScript
    , ScriptResult(..)
) where

import System.Directory     (getCurrentDirectory)
import System.Exit          (ExitCode(..))
import System.FilePath      ((</>), isAbsolute)
import System.IO.Temp       (getCanonicalTemporaryDirectory)
import System.Process.Typed (runProcess, shell)

type PythonScript = String

data ScriptResult = ScriptSuccess 
                  | ScriptFailure Int

-- | Take a python script in string form, write it in a temporary directory,
-- then execute it. 
runTempPythonScript :: PythonScript    -- ^ Content of the script
                    -> IO ScriptResult -- ^ Result with exit code.
runTempPythonScript script = do
            -- Write script to temporary directory
            scriptPath <- (</> "pandoc-pyplot.py") <$>  getCanonicalTemporaryDirectory
            writeFile scriptPath script
            -- Execute script
            ec <- runProcess $ shell $ "python " <> (show scriptPath)
            case ec of
                ExitSuccess -> return ScriptSuccess
                ExitFailure code -> return $ ScriptFailure code

-- | Modify a Python plotting script to save the figure to a filename.
addPlotCapture :: FilePath          -- ^ Path where to save the figure
               -> PythonScript      -- ^ Raw code block
               -> IO PythonScript   -- ^ Code block with added capture
addPlotCapture fname content = do
    absFname <- if isAbsolute fname
                then (return fname)
                else (</> fname) <$> getCurrentDirectory
    return $ mconcat [ content
                     , "\nimport matplotlib.pyplot as plt"  -- Just in case
                     , "\nplt.savefig(" <> show absFname <> ")\n\n"
                     ]
