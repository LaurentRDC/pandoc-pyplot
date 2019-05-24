{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Text.Pandoc.Filter.Pyplot.Scripting
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types and functions that help
with running Python scripts.
-}
module Text.Pandoc.Filter.Pyplot.Scripting
    ( runTempPythonScript
    , runScriptIfNecessary
    , hasBlockingShowCall
    ) where

import           Data.Hashable        (hash)
import           Data.List            (intersperse)
import           Data.Monoid          (Any(..))
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import           System.Directory     (createDirectoryIfMissing,
                                      doesFileExist)
import           System.Exit          (ExitCode (..))
import           System.FilePath      ((</>), takeDirectory)
import           System.IO.Temp       (getCanonicalTemporaryDirectory)
import           System.Process.Typed (runProcess, shell)

import           Text.Pandoc.Filter.Pyplot.Types
import           Text.Pandoc.Filter.Pyplot.FigureSpec

-- | Take a python script in string form, write it in a temporary directory,
-- then execute it.
runTempPythonScript :: String          -- ^ Interpreter (e.g. "python" or "python35")
                    -> [String]        -- ^ Command-line flags
                    -> PythonScript    -- ^ Content of the script
                    -> IO ScriptResult -- ^ Result with exit code.
runTempPythonScript interpreter' flags' script' = do
    -- We involve the script hash as a temporary filename
    -- so that there is never any collision
    scriptPath <- (</> hashedPath) <$> getCanonicalTemporaryDirectory
    T.writeFile scriptPath script'

    let command = mconcat . intersperse " " $ [interpreter'] <> flags' <> [show scriptPath]

    ec <- runProcess . shell $ command
    case ec of
        ExitSuccess      -> return   ScriptSuccess
        ExitFailure code -> return $ ScriptFailure code
    where
        hashedPath = show . hash $ script'

-- | Run the Python script. In case the file already exists, we can safely assume
-- there is no need to re-run it.
runScriptIfNecessary :: Configuration -> FigureSpec -> IO ScriptResult
runScriptIfNecessary config spec = do
    createDirectoryIfMissing True . takeDirectory $ figurePath spec
    fileAlreadyExists <- doesFileExist $ figurePath spec
    result <- if fileAlreadyExists
                then return ScriptSuccess
                else runTempPythonScript (interpreter config) (flags config) (addPlotCapture spec)
    case result of
        ScriptFailure code -> return $ ScriptFailure code
        ScriptSuccess      -> T.writeFile (sourceCodePath spec) (script spec) >> return ScriptSuccess

-- | Detect the presence of a blocking show call, for example "plt.show()"
hasBlockingShowCall :: PythonScript -> Bool
hasBlockingShowCall script' =
    anyOf
        [ "plt.show()" `elem` scriptLines
        , "pyplot.show()" `elem` scriptLines
        , "matplotlib.pyplot.show()" `elem` scriptLines
        ]
  where
    scriptLines = T.lines script'
    anyOf xs = getAny $ mconcat $ Any <$> xs