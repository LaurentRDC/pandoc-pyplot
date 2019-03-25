{-# LANGUAGE Unsafe            #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Filter.Scripting
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types and functions that help
with running Python scripts.
-}

module Text.Pandoc.Filter.Scripting (
      runTempPythonScript
    , addPlotCapture
    , hasBlockingShowCall
    , PythonScript
    , ScriptResult(..)
) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import           System.Exit          (ExitCode (..))
import           System.FilePath      ((</>))
import           System.IO.Temp       (getCanonicalTemporaryDirectory)
import           System.Process.Typed (runProcess, shell)

import           Data.Monoid          (Any (..), (<>))

-- | String representation of a Python script
type PythonScript = Text

-- | Possible result of running a Python script
data ScriptResult = ScriptSuccess
                  | ScriptFailure Int

-- | Take a python script in string form, write it in a temporary directory,
-- then execute it.
runTempPythonScript :: PythonScript    -- ^ Content of the script
                    -> IO ScriptResult -- ^ Result with exit code.
runTempPythonScript script = do
            -- Write script to temporary directory
            scriptPath <- (</> "pandoc-pyplot.py") <$> getCanonicalTemporaryDirectory
            T.writeFile scriptPath script
            -- Execute script
            ec <- runProcess $ shell $ "python -OO " <> (show scriptPath)
            case ec of
                ExitSuccess      -> return ScriptSuccess
                ExitFailure code -> return $ ScriptFailure code

-- | Modify a Python plotting script to save the figure to a filename.
-- TODO: add plot capture for hires PNG and PDF
addPlotCapture :: FilePath          -- ^ Path where to save the figure
               -> Int               -- ^ DPI
               -> PythonScript      -- ^ Raw code block
               -> PythonScript      -- ^ Code block with added capture
addPlotCapture fname dpi content =
    mconcat [ content
            , "\nimport matplotlib.pyplot as plt"  -- Just in case
            , mconcat [ "\nplt.savefig("
                      , T.pack $ show fname -- show is required for quotes
                      , ", dpi=", T.pack $ show dpi
                      , ")\n\n"]
            ]

-- | Detect the presence of a blocking show call, for example "plt.show()"
hasBlockingShowCall :: PythonScript -> Bool
hasBlockingShowCall script = anyOf
        [ "plt.show()" `elem` scriptLines
        , "matplotlib.pyplot.show()" `elem` scriptLines
        ]
    where
        scriptLines = T.lines script
        anyOf xs = getAny $ mconcat $ Any <$> xs
