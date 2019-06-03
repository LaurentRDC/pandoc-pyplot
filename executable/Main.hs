{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative       ((<|>))

import           Data.Default.Class        (def)
import           Data.List                 (intersperse)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T

import           Options.Applicative
import qualified Options.Applicative.Help.Pretty as P

import           System.Directory          (doesFileExist)
import           System.IO.Temp            (writeSystemTempFile)

import           Text.Pandoc.Filter.Pyplot (plotTransformWithConfig, configuration, SaveFormat(..))
import           Text.Pandoc.JSON          (toJSONFilter)

import           Web.Browser               (openBrowser)

import qualified Data.Version              as V
import           Paths_pandoc_pyplot       (version)

import           ManPage                   (embedManualHtml)

main :: IO ()
main = do
    configExists <- doesFileExist ".pandoc-pyplot.yml"
    config <- if configExists
                then configuration ".pandoc-pyplot.yml"
                else def
    
    options <- execParser opts
    case options of
        Just f  -> flagAction f
        Nothing -> toJSONFilter (plotTransformWithConfig config)
        

data Flag = Version
          | Formats
          | Manual
    deriving (Eq)

versionP :: Parser (Maybe Flag)
versionP = flag Nothing (Just Version) 
    (long "version" 
    <> short 'v' 
    <> help "Show version number and exit."
    )

formatsP :: Parser (Maybe Flag)
formatsP = flag Nothing (Just Formats) 
    (long "formats" 
    <> short 'f' 
    <> help "Show supported output figure formats and exit."
    )

manualP :: Parser (Maybe Flag)
manualP = flag Nothing (Just Manual) 
    (long "manual" 
    <> short 'm' 
    <> help "Open the manual page in the default web browser and exit."
    )

optionsParser :: Parser (Maybe Flag)
optionsParser = versionP <|> formatsP <|> manualP

opts :: ParserInfo (Maybe Flag)
opts = info (optionsParser <**> helper)
    (fullDesc
    <> progDesc "This pandoc filter generates plots from Python code blocks using Matplotlib. This allows to keep documentation and figures in perfect synchronicity."
    <> header "pandoc-pyplot - generate Matplotlib figures directly in documents."
    <> footerDoc (Just footer')
    )

flagAction :: Flag -> IO ()
flagAction f
    | f == Version = showVersion
    | f == Formats = showFormats
    | f == Manual  = showManual
    | otherwise    = error "Unknown flag"
    where   
        showVersion = putStrLn (V.showVersion version)
        showFormats = putStrLn . mconcat . intersperse ", " . fmap show $ supportedSaveFormats
        showManual  = writeSystemTempFile "pandoc-pyplot-manual.html" (T.unpack manualHtml) 
                        >>= \fp -> openBrowser ("file:///" <> fp) >> return ()

supportedSaveFormats :: [SaveFormat]
supportedSaveFormats = enumFromTo minBound maxBound

manualHtml :: T.Text
manualHtml = T.pack $(embedManualHtml)

-- | Use Doc type directly because of newline formatting
footer' :: P.Doc
footer' = mconcat [
        P.text "Example usage with pandoc:"
    , P.line, P.line
    , P.indent 4 $ P.string "> pandoc --filter pandoc-pyplot input.md --output output.html"
    , P.line, P.line
    , P.text "If you use pandoc-pyplot in combination with other filters, you probably want to run pandoc-pyplot first. Here is an example with pandoc-crossref:"
    , P.line, P.line
    , P.indent 4 $ P.string "> pandoc --filter pandoc-pyplot --filter pandoc-crossref -i input.md -o output.pdf"
    , P.line, P.line
    , P.text "More information can be found via the manual (pandoc-pyplot --manual) or the repository README, located at"
    , P.line
    , P.indent 4 $ P.text "https://github.com/LaurentRDC/pandoc-pyplot"
    , P.line
    ]