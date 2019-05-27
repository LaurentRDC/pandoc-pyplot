{-# LANGUAGE CPP #-}
{-|
Module      : Text.Pandoc.Filter.Pyplot.Types
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types in use in pandoc-pyplot
-}

module Text.Pandoc.Filter.Pyplot.Types where

import Data.Char              (toLower)
import Data.Default.Class     (Default, def)
import Data.Hashable          (Hashable, hashWithSalt)
import Data.Semigroup         as Sem
import Data.Text              (Text)

import Text.Pandoc.Definition (Attr)   

-- | String representation of a Python script
type PythonScript = Text

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
    (<>) CheckPassed a = a
    (<>) a CheckPassed = a
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
    deriving (Bounded, Enum, Eq, Show)

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

-- | Configuration of pandoc-pyplot, describing the default behavior
-- of the filter. 
--
-- A Configuration is useful when dealing with lots of figures; it avoids
-- repeating the same values.sta
-- 
-- @since 2.1.0.0
data Configuration 
    = Configuration 
        { defaultDirectory     :: FilePath     -- ^ The default directory where figures will be saved.
        , defaultIncludeScript :: PythonScript -- ^ The default script to run before other instructions.
        , defaultSaveFormat    :: SaveFormat   -- ^ The default save format of generated figures.
        , defaultDPI           :: Int          -- ^ The default dots-per-inch value for generated figures.
        , interpreter          :: String       -- ^ The name of the interpreter to use to render figures.
        , flags                :: [String]     -- ^ Command-line flags to be passed to the Python interpreger, e.g. ["-O", "-Wignore"]
        }
    deriving (Eq, Show)

instance Default Configuration where
    def = Configuration {
          defaultDirectory     = "generated/"
        , defaultIncludeScript = mempty
        , defaultSaveFormat    = PNG
        , defaultDPI           = 80
        , interpreter          = "python"
        , flags                = mempty
    }

-- | Datatype containing all parameters required to run pandoc-pyplot
data FigureSpec = FigureSpec
    { caption    :: String       -- ^ Figure caption.
    , script     :: PythonScript -- ^ Source code for the figure.
    , saveFormat :: SaveFormat   -- ^ Save format of the figure
    , directory  :: FilePath     -- ^ Directory where to save the file
    , dpi        :: Int          -- ^ Dots-per-inch of figure
    , blockAttrs :: Attr         -- ^ Attributes not related to @pandoc-pyplot@ will be propagated.
    }

instance Hashable FigureSpec where
    hashWithSalt salt spec =
        hashWithSalt salt ( caption spec
                          , script spec
                          , fromEnum . saveFormat $ spec
                          , directory spec, dpi spec
                          , blockAttrs spec
                          )
