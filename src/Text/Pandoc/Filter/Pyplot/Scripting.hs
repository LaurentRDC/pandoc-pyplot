{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : $header$
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
    ) where

import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class

import           Data.Hashable        (hash)
import           Data.List            (intersperse)
import           Data.Monoid          (Any(..), (<>))
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

-- | Detect the presence of a blocking show call, for example "plt.show()"
checkBlockingShowCall :: PythonScript -> CheckResult
checkBlockingShowCall script' = 
    if hasShowCall 
        then CheckFailed "The script has a blocking call to `matplotlib.pyplot.show`. "
        else CheckPassed 
    where
        scriptLines = T.lines script'
        hasShowCall = getAny $ mconcat $ Any <$>
            [ "plt.show()" `elem` scriptLines
            , "pyplot.show()" `elem` scriptLines
            , "matplotlib.pyplot.show()" `elem` scriptLines
            ]

-- | List of all script checks
-- This might be overkill right now but extension to other languages will be easier
scriptChecks :: [PythonScript -> CheckResult]
scriptChecks = [checkBlockingShowCall]

-- | Take a python script in string form, write it in a temporary directory,
-- then execute it.
runTempPythonScript :: PythonScript         -- ^ Content of the script
                    -> PyplotM ScriptResult -- ^ Result.
runTempPythonScript script' =  case checkResult of
    CheckFailed msg -> return $ ScriptChecksFailed msg
    CheckPassed -> do
        -- We involve the script hash as a temporary filename
        -- so that there is never any collision
        scriptPath <- liftIO $ (</> hashedPath) <$> getCanonicalTemporaryDirectory
        liftIO $ T.writeFile scriptPath script'
        interpreter' <- asks interpreter
        flags' <- asks flags
        let command = mconcat . intersperse " " $ [interpreter'] <> flags' <> [show scriptPath]

        ec <- liftIO $ runProcess . shell $ command
        case ec of
            ExitSuccess      -> return   ScriptSuccess
            ExitFailure code -> return $ ScriptFailure code
    where
        checkResult = mconcat $ scriptChecks <*> [script']
        hashedPath = show . hash $ script'

-- | Run the Python script. In case the file already exists, we can safely assume
-- there is no need to re-run it.
runScriptIfNecessary :: FigureSpec 
                     -> PyplotM ScriptResult
runScriptIfNecessary spec = do
    liftIO $ createDirectoryIfMissing True . takeDirectory $ figurePath spec

    fileAlreadyExists <- liftIO . doesFileExist $ figurePath spec
    result <- if fileAlreadyExists
                then return ScriptSuccess
                else runTempPythonScript scriptWithCapture
    
    case result of
        ScriptSuccess      -> liftIO $ T.writeFile (sourceCodePath spec) (script spec) >> return ScriptSuccess
        ScriptFailure code -> return $ ScriptFailure code
        ScriptChecksFailed msg -> return $ ScriptChecksFailed msg

    where
        scriptWithCapture = addPlotCapture spec