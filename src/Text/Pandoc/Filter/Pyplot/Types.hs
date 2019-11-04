{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types in use in pandoc-pyplot
-}

module Text.Pandoc.Filter.Pyplot.Types where

import           Control.Monad.Reader

import           Data.Char              (toLower)
import           Data.Default.Class     (Default, def)
import           Data.Hashable          (Hashable)
import           Data.Semigroup         as Sem
import           Data.Text              (Text, pack)
import           Data.Yaml

import           GHC.Generics           (Generic)

import           Text.Pandoc.Definition (Attr)


-- | Keys that pandoc-pyplot will look for in code blocks. These are only exported for testing purposes.
directoryKey, captionKey, dpiKey, includePathKey, saveFormatKey, withLinksKey, isTightBboxKey, isTransparentKey :: String
directoryKey     = "directory"
captionKey       = "caption"
dpiKey           = "dpi"
includePathKey   = "include"
saveFormatKey    = "format"
withLinksKey     = "links"
isTightBboxKey   = "tight_bbox"
isTransparentKey = "transparent"

-- | list of all keys related to pandoc-pyplot that
-- can be specified in source material.
inclusionKeys :: [String]
inclusionKeys = [ directoryKey
                , captionKey
                , dpiKey
                , includePathKey
                , saveFormatKey
                , withLinksKey
                , isTightBboxKey
                , isTransparentKey
                ]


-- | Monad in which to run pandoc-pyplot computations
type PyplotM a = ReaderT Configuration IO a


-- | String representation of a Python script
type PythonScript = Text


-- | Rendering library
--
-- @since 2.2.0.0
data RenderingLibrary = Matplotlib
                      | Plotly
    deriving (Show, Eq, Generic)

instance Hashable RenderingLibrary

instance ToJSON RenderingLibrary
instance FromJSON RenderingLibrary


-- | Possible result of running a Python script
data ScriptResult
    = ScriptSuccess
    | ScriptChecksFailed String
    | ScriptFailure Int


-- | Result of checking scripts for problems
data CheckResult
    = CheckPassed
    | CheckFailed String
    deriving (Eq)

instance Sem.Semigroup CheckResult where
    (<>) CheckPassed a                         = a
    (<>) a CheckPassed                         = a
    (<>) (CheckFailed msg1) (CheckFailed msg2) = CheckFailed (msg1 <> msg2)

instance Monoid CheckResult where
    mempty = CheckPassed

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif


-- | Possible errors returned by the filter
data PandocPyplotError
    = ScriptError Int                 -- ^ Running Python script has yielded an error
    | ScriptChecksFailedError String  -- ^ Python script did not pass all checks
    deriving (Eq)

instance Show PandocPyplotError where
    show (ScriptError exitcode)        = "Script error: plot could not be generated. Exit code " <> (show exitcode)
    show (ScriptChecksFailedError msg) = "Script did not pass all checks: " <> msg


-- | Generated figure file format supported by pandoc-pyplot.
data SaveFormat
    = PNG
    | PDF
    | SVG
    | JPG
    | EPS
    | GIF
    | TIF
    deriving (Bounded, Enum, Eq, Show, Generic)

instance Hashable SaveFormat -- From Generic

-- | Parse an image save format string
--
-- >>> saveFormatFromString ".png"
-- Just PNG
--
-- >>> saveFormatFromString "jpeg"
-- Just JPEG
--
-- >>> SaveFormatFromString "arbitrary"
-- Nothing
saveFormatFromString :: String -> Maybe SaveFormat
saveFormatFromString s
    | s `elem` ["png", "PNG", ".png"] = Just PNG
    | s `elem` ["pdf", "PDF", ".pdf"] = Just PDF
    | s `elem` ["svg", "SVG", ".svg"] = Just SVG
    | s `elem` ["eps", "EPS", ".eps"] = Just EPS
    | s `elem` ["gif", "GIF", ".gif"] = Just GIF
    | s `elem` ["jpg", "jpeg", "JPG", "JPEG", ".jpg", ".jpeg"] = Just JPG
    | s `elem` ["tif", "tiff", "TIF", "TIFF", ".tif", ".tiff"] = Just TIF
    | otherwise = Nothing

-- | Save format file extension
extension :: SaveFormat -> String
extension fmt = mconcat [".", fmap toLower . show $ fmt]

-- | Default interpreter should be Python 3, which has a different
-- name on Windows ("python") vs Unix ("python3")
--
-- @since 2.1.2.0
defaultPlatformInterpreter :: String
#if defined(mingw32_HOST_OS)
defaultPlatformInterpreter = "python"
#else
defaultPlatformInterpreter = "python3"
#endif

-- | Configuration of pandoc-pyplot, describing the default behavior
-- of the filter.
--
-- A Configuration is useful when dealing with lots of figures; it avoids
-- repeating the same values.sta
--
-- @since 2.1.0.0
data Configuration
    = Configuration
        { defaultDirectory     :: FilePath         -- ^ The default directory where figures will be saved.
        , defaultIncludeScript :: PythonScript     -- ^ The default script to run before other instructions.
        , defaultWithLinks     :: Bool             -- ^ The default behavior of whether or not to include links to source code and high-res
        , defaultSaveFormat    :: SaveFormat       -- ^ The default save format of generated figures.
        , defaultDPI           :: Int              -- ^ The default dots-per-inch value for generated figures.
        , renderingLibrary     :: RenderingLibrary -- ^ Rendering library to use in producing the figure.
        , isTightBbox          :: Bool             -- ^ Whether the figures should be saved with @bbox_inches="tight"@ or not. Useful for larger figures with subplots.
        , isTransparent        :: Bool             -- ^ If True, figures will be saved with transparent background rather than solid color.
        , interpreter          :: String           -- ^ The name of the interpreter to use to render figures.
        , flags                :: [String]         -- ^ Command-line flags to be passed to the Python interpreger, e.g. ["-O", "-Wignore"]
        }
    deriving (Eq, Show)

instance Default Configuration where
    def = Configuration {
          defaultDirectory     = "generated/"
        , defaultIncludeScript = mempty
        , defaultWithLinks     = True
        , defaultSaveFormat    = PNG
        , defaultDPI           = 80
        , renderingLibrary     = Matplotlib
        , isTightBbox          = False
        , isTransparent        = False
        , interpreter          = defaultPlatformInterpreter
        , flags                = mempty
    }

instance ToJSON Configuration where
    toJSON (Configuration dir' _ withLinks' savefmt' dpi' renderingLib' tightbbox' transparent' interp' flags') =
        -- We ignore the include script as we want to examplify that
        -- this is for a filepath
            object [ pack directoryKey     .= dir'
                   , pack includePathKey   .= ("example.py" :: FilePath)
                   , pack withLinksKey     .= withLinks'
                   , pack dpiKey           .= dpi'
                   , pack saveFormatKey    .= (toLower <$> show savefmt')
                   , pack isTightBboxKey   .= tightbbox'
                   , pack isTransparentKey .= transparent'
                   , "rendering_library"   .= renderingLib'
                   , "interpreter"         .= interp'
                   , "flags"               .= flags'
                   ]


-- | Datatype containing all parameters required to run pandoc-pyplot.
--
-- It is assumed that once a @FigureSpec@ has been created, no configuration
-- can overload it; hence, a @FigureSpec@ completely encodes a particular figure.
data FigureSpec = FigureSpec
    { caption      :: String           -- ^ Figure caption.
    , withLinks    :: Bool             -- ^ Append links to source code and high-dpi figure to caption.
    , script       :: PythonScript     -- ^ Source code for the figure.
    , saveFormat   :: SaveFormat       -- ^ Save format of the figure.
    , directory    :: FilePath         -- ^ Directory where to save the file.
    , dpi          :: Int              -- ^ Dots-per-inch of figure.
    , renderingLib :: RenderingLibrary -- ^ Rendering library.
    , tightBbox    :: Bool             -- ^ Enforce tight bounding-box with @bbox_inches="tight"@.
    , transparent  :: Bool             -- ^ Make figure background transparent.
    , blockAttrs   :: Attr             -- ^ Attributes not related to @pandoc-pyplot@ will be propagated.
    } deriving Generic

instance Hashable FigureSpec -- From Generic
