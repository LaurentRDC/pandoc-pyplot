{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Default.Class        (def)
import           System.Environment        (getArgs)
import           System.Directory          (doesFileExist)

import           Text.Pandoc.Filter.Pyplot (makePlotWithConfig, configuration)
import           Text.Pandoc.JSON          (toJSONFilter)

import qualified Data.Version              as V
import           Paths_pandoc_pyplot       (version)

-- The formatting is borrowed from Python's argparse library
help :: String
help =
    "\n\
    \\n\
    \   usage: pandoc-pyplot [-h, --help] [-v, --version] \n\
    \\n\
    \   This pandoc filter generates plots from Python code blocks using Matplotlib. \n\
    \   This allows to keep documentation and figures in perfect synchronicity.\n\
    \\n\
    \   Optional arguments:\n\
    \       -h, --help     Show this help message and exit\n\
    \       -v, --version  Show version number and exit \n\
    \\n\
    \   To use with pandoc: \n\
    \       pandoc --filter pandoc-pyplot input.md --output output.html\n\
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
            | arg `elem` ["-h", "--help"] -> showHelp
            | arg `elem` ["-v", "--version"] -> showVersion
        _ -> toJSONFilter (makePlotWithConfig config)
  where
    showHelp = putStrLn help
    showVersion = putStrLn (V.showVersion version)
