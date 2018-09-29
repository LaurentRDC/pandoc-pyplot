
module Main where

import Data.List                 (intersperse)
import System.Environment        (getArgs)
import System.Exit               (die)

import Text.Pandoc.JSON          (toJSONFilter)
import Text.Pandoc.Filter.Pyplot (makePlot)

import qualified Data.Version as V
import Paths_pandoc_pyplot       (version)

-- The formatting is borrowed from Python's argparse library
help :: String
help = "\n\
    \\n\
    \   usage: pandoc-pyplot [-h, --help] [-v, --version] \n\
    \\n\
    \   This pandoc filter generates plots from Python code blocks using Matplotlib. \n\
    \   This allows to keep documentation and figures up-to-date.\n\
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
    args <- getArgs
    case args of
        [] -> toJSONFilter makePlot 
        (arg:_)
            | arg `elem` ["-h", "--help"]    -> showHelp
            | arg `elem` ["-v", "--version"] -> showVersion
            | otherwise                      -> die $ "Invalid argument(s): " <> (showArgs args)
    where
        showHelp    = putStrLn help
        showVersion = putStrLn (V.showVersion version)
        showArgs    = (mconcat . intersperse ", ")
