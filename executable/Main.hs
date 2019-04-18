{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.Default.Class        (def)
import           Data.List                 (intersperse)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

import           System.Environment        (getArgs)
import           System.Directory          (doesFileExist)

import           Text.Pandoc.Filter.Pyplot (plotTransformWithConfig, configuration, SaveFormat(..))
import           Text.Pandoc.JSON          (toJSONFilter)

import qualified Data.Version              as V
import           Paths_pandoc_pyplot       (version)

import           ManPage                   (embedManualText, embedManualHtml)

supportedSaveFormats :: [SaveFormat]
supportedSaveFormats = enumFromTo minBound maxBound

manualText, manualHtml :: T.Text
manualText = T.pack $(embedManualText)
manualHtml = T.pack $(embedManualHtml)

-- The formatting is borrowed from Python's argparse library
help :: String
help =
    "\n\
    \\n\
    \   usage: pandoc-pyplot [-h, --help] [-v, --version] [-f, --formats] [-m, --manual] \n\
    \\n\
    \   This pandoc filter generates plots from Python code blocks using Matplotlib. \n\
    \   This allows to keep documentation and figures in perfect synchronicity.\n\
    \\n\
    \   Optional arguments:\n\
    \       -h, --help     Show this help message and exit\n\
    \       -v, --version  Show version number and exit \n\
    \       -f, --formats  Show supported output figure formats and exit \n\
    \       -m, --manual   Show the manual page \n\
    \\n\
    \   To use with pandoc: \n\
    \       pandoc -s --filter pandoc-pyplot input.md --output output.html\n\
    \\n\
    \   More information can be found in the repository README, located at \n\
    \       https://github.com/LaurentRDC/pandoc-pyplot\n"

main :: IO ()
main = do
    configExists <- doesFileExist ".pandoc-pyplot.yml"
    config <- if configExists
                then configuration ".pandoc-pyplot.yml"
                else def

    getArgs >>= \case
        (arg:_)
            | arg `elem` ["-h", "--help"]    -> showHelp
            | arg `elem` ["-v", "--version"] -> showVersion
            | arg `elem` ["-f", "--formats"] -> showFormats
            | arg `elem` ["-m", "--manual"]  -> showManual
        _ -> toJSONFilter (plotTransformWithConfig config)
  where
    showHelp    = putStrLn help
    showVersion = putStrLn (V.showVersion version)
    showFormats = putStrLn . mconcat . intersperse ", " . fmap show $ supportedSaveFormats
    showManual  = T.putStrLn manualText
