
module Text.Pandoc.Filter.Scripting (
      runTempPythonScript
    , addPlotCapture
    , hasBlockingShowCall
    , PythonScript
    , ScriptResult(..)
) where

import System.Directory     (getCurrentDirectory)
import System.Exit          (ExitCode(..))
import System.FilePath      ((</>), isAbsolute)
import System.IO.Temp       (getCanonicalTemporaryDirectory)
import System.Process.Typed (runProcess, shell)

import Data.Monoid          (Any(..))

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

-- | Detect the presence of a blocking show call, for example "plt.show()"
hasBlockingShowCall :: PythonScript -> Bool
hasBlockingShowCall script = anyOf
        [ "plt.show()" `elem` scriptLines
        , "matplotlib.pyplot.show()" `elem` scriptLines
        ]
    where
        scriptLines = lines script
        anyOf xs = getAny $ mconcat $ Any <$> xs