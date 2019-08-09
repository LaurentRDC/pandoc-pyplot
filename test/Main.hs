{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                 (unless)

import           Data.Default.Class            (def)
import           Data.List                     (isInfixOf, isSuffixOf)
import           Data.Monoid                   ((<>))
import           Data.Text                     (unpack)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.Pandoc.Filter.Pyplot
import           Text.Pandoc.Filter.Pyplot.Internal

import           Text.Pandoc.JSON
import qualified Text.Pandoc.Builder           as B
import qualified Text.Pandoc.Definition        as B

import           System.Directory              (createDirectory,
                                                createDirectoryIfMissing,
                                                doesDirectoryExist,
                                                doesFileExist, listDirectory,
                                                removeDirectoryRecursive,
                                                removePathForcibly)
import           System.FilePath               ((</>), takeExtensions)
import           System.IO.Temp                (getCanonicalTemporaryDirectory)

main :: IO ()
main =
    defaultMain $
    testGroup
        "Text.Pandoc.Filter.Pyplot"
        [ testFileCreation
        , testFileInclusion
        , testSaveFormat
        , testBlockingCallError
        , testMarkdownFormattingCaption
        , testWithLinks
        , testWithConfiguration
        , testOverridingConfiguration
        , testBuildConfiguration
        ]

plotCodeBlock :: PythonScript -> Block
plotCodeBlock script = CodeBlock (mempty, ["pyplot"], mempty) (unpack script)

addCaption :: String -> Block -> Block
addCaption caption (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(captionKey, caption)]) script

addDirectory :: FilePath -> Block -> Block
addDirectory dir (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(directoryKey, dir)]) script

addInclusion :: FilePath -> Block -> Block
addInclusion inclusionPath (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(includePathKey, inclusionPath)]) script

addSaveFormat :: SaveFormat -> Block -> Block
addSaveFormat saveFormat (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(saveFormatKey, extension saveFormat)]) script

addDPI :: Int -> Block -> Block
addDPI dpi (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(dpiKey, show dpi)]) script

addWithLinks :: Bool -> Block -> Block
addWithLinks yn (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(withLinksKey, show yn)]) script


-- | Assert that a file exists
assertFileExists :: HasCallStack => FilePath -> Assertion
assertFileExists filepath = do
    fileExists <- doesFileExist filepath
    unless fileExists (assertFailure msg)
  where
    msg = mconcat ["File ", filepath, " does not exist."]

-- | Not available with GHC < 8.4
-- since this function was added in filepath-1.4.2
-- but GHC 8.2.2 comes with filepath-1.4.1.2
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
    

-- | Assert that the first list is contained,
-- wholly and intact, anywhere within the second.
assertIsInfix :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> Assertion
assertIsInfix xs ys = unless (xs `isInfixOf` ys) (assertFailure msg)
  where
    msg = mconcat ["Expected ", show xs, " to be an infix of ", show ys]

-- Ensure a directory is empty but exists.
ensureDirectoryExistsAndEmpty :: FilePath -> IO ()
ensureDirectoryExistsAndEmpty dir = do
    exists <- doesDirectoryExist dir
    if exists
        then removePathForcibly dir
        else return ()
    createDirectory dir

-------------------------------------------------------------------------------
-- Test that plot files and source files are created when the filter is run
testFileCreation :: TestTree
testFileCreation =
    testCase "writes output files in appropriate directory" $ do
        tempDir <- (</> "test-file-creation") <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let codeBlock = (addDirectory tempDir $ plotCodeBlock "import matplotlib.pyplot as plt\n")
        _ <- makePlot' def codeBlock
        filesCreated <- length <$> listDirectory tempDir
        assertEqual "" filesCreated 3

-------------------------------------------------------------------------------
-- Test that included files are found within the source
testFileInclusion :: TestTree
testFileInclusion =
    testCase "includes plot inclusions" $ do
        tempDir <- (</> "test-file-inclusion") <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let codeBlock =
                (addInclusion "test/fixtures/include.py" $
                 addDirectory tempDir $ plotCodeBlock "import matplotlib.pyplot as plt\n")
        _ <- makePlot' def codeBlock
        inclusion <- readFile "test/fixtures/include.py"
        sourcePath <- head . filter (isExtensionOf "txt") <$> listDirectory tempDir
        src <- readFile (tempDir </> sourcePath)
        assertIsInfix inclusion src

-------------------------------------------------------------------------------
-- Test that the files are saved in the appropriate format
testSaveFormat :: TestTree
testSaveFormat =
    testCase "saves in the appropriate format" $ do
        tempDir <- (</> "test-safe-format") <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let codeBlock =
                (addSaveFormat JPG $
                 addDirectory tempDir $
                 plotCodeBlock
                     "import matplotlib.pyplot as plt\nplt.figure()\nplt.plot([1,2], [1,2])")
        _ <- makePlot' def codeBlock
        numberjpgFiles <-
            length <$> filter (isExtensionOf (extension JPG)) <$>
            listDirectory tempDir
        assertEqual "" numberjpgFiles 2

-------------------------------------------------------------------------------
-- Test that a script containing a blocking call to matplotlib.pyplot.show
-- returns the appropriate error
testBlockingCallError :: TestTree
testBlockingCallError =
    testCase "raises an exception for blocking calls" $ do
        tempDir <- (</> "test-blocking-call-error") <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let codeBlock = addDirectory tempDir $ plotCodeBlock "import matplotlib.pyplot as plt\nplt.show()"
        result <- makePlot' def codeBlock
        case result of
            Right block -> assertFailure "did not catch the expected blocking call"
            Left error ->
                if isCheckError error
                    then pure ()
                    else assertFailure "An error was caught but not the expected blocking call"
    where
        isCheckError (ScriptChecksFailedError msg) = True
        isCheckError _ = False
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test that Markdown formatting in captions is correctly rendered
testMarkdownFormattingCaption :: TestTree
testMarkdownFormattingCaption =
    testCase "appropriately parses Markdown captions" $ do
        tempDir <- (</> "test-caption-parsing") <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        -- Note that this test is fragile, in the sense that the expected result must be carefully
        -- constructed
        let expected = [B.Strong [B.Str "caption"]]
            codeBlock = addDirectory tempDir $ addCaption "**caption**" $ plotCodeBlock "import matplotlib.pyplot as plt"
        result <- makePlot' def codeBlock
        case result of
            Left error -> assertFailure $ "an error occured: " <> show error
            Right block -> assertIsInfix expected (extractCaption block)
    where
        extractCaption (B.Para blocks) = extractImageCaption . head $ blocks
        extractCaption _ = mempty

        extractImageCaption (Image _ c _) = c
        extractImageCaption _ = mempty
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test that it is possible to not render links in captions
testWithLinks :: TestTree
testWithLinks =
    testCase "appropriately omits links to source code and high-res image" $ do
        tempDir <- (</> "test-caption-links") <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        -- Note that this test is fragile, in the sense that the expected result must be carefully
        -- constructed
        let expected = mempty
            codeBlock = addWithLinks False $ addDirectory tempDir $ addCaption mempty $ plotCodeBlock "import matplotlib.pyplot as plt"
        result <- makePlot' def codeBlock
        case result of
            Left error -> assertFailure $ "an error occured: " <> show error
            Right block -> assertIsInfix expected (extractCaption block)
    where
        extractCaption (B.Para blocks) = extractImageCaption . head $ blocks
        extractCaption _ = mempty

        extractImageCaption (Image _ c _) = c
        extractImageCaption _ = mempty
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Test with configuration
testConfig :: IO Configuration
testConfig = do
    tempDir <- (</> "test-with-config") <$> getCanonicalTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    return $ def {defaultDirectory = tempDir, defaultSaveFormat = JPG}

testOverridingConfiguration :: TestTree
testOverridingConfiguration =
    testCase "follows the configuration options" $ do
        config <- testConfig

        -- The default from config says the save format should be JPG
        -- but the code block save format="png"
        let codeBlock = (addSaveFormat PNG $ 
                         plotCodeBlock 
                            "import matplotlib.pyplot as plt\nplt.figure()\nplt.plot([1,2], [1,2])")
        _ <- makePlot' config codeBlock

        numberjpgFiles <-
            length <$> filter (isExtensionOf (extension JPG)) <$>
            listDirectory (defaultDirectory config)
        numberpngFiles <-
            length <$> filter (isExtensionOf (extension PNG)) <$>
            listDirectory (defaultDirectory config)
        assertEqual "" numberjpgFiles 0
        assertEqual "" numberpngFiles 2
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test that values in code blocks will override the defaults in configuration
testWithConfiguration :: TestTree
testWithConfiguration =
    testCase "code block attributes override configuration defaults" $ do
        config <- testConfig

        let codeBlock = plotCodeBlock "import matplotlib.pyplot as plt\nplt.figure()\nplt.plot([1,2], [1,2])"
        _ <- makePlot' config codeBlock

        numberjpgFiles <-
            length <$> filter (isExtensionOf (extension JPG)) <$>
            listDirectory (defaultDirectory config)
        assertEqual "" numberjpgFiles 2
-------------------------------------------------------------------------------

testBuildConfiguration :: TestTree
testBuildConfiguration = 
    testCase "configuration is correctly parsed" $ do
        let config = def { defaultDirectory = "generated/other"
                         , defaultSaveFormat = JPG
                         , defaultDPI = 150
                         , flags = ["-Wignore"]
<<<<<<< HEAD
=======
                         , isTightBbox = True
                         , isTransparent = True
>>>>>>> 6fe32e3bd0fa418c94890d72ca0345736575ab2a
                         }
        parsedConfig <- configuration "test/fixtures/.pandoc-pyplot.yml"
        assertEqual "" config parsedConfig