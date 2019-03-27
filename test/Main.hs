{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad     (unless)

import Data.List         (isInfixOf)
import Data.Text         (unpack)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Pandoc.Filter.Pyplot as Filter
import qualified Text.Pandoc.Filter.Scripting as Filter
import Text.Pandoc.JSON

import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , listDirectory
                        , createDirectoryIfMissing
                        , createDirectory
                        , removeDirectoryRecursive
                        , removePathForcibly 
                        )
import System.FilePath  ((</>), isExtensionOf)
import System.IO.Temp   (getCanonicalTemporaryDirectory)

main :: IO ()
main = defaultMain $ 
    testGroup "Text.Pandoc.Filter.Pyplot" 
        [ testFileCreation
        , testFileInclusion
        , testBlockingCallError
        ]

plotCodeBlock :: Filter.PythonScript -> Block
plotCodeBlock script = CodeBlock (mempty, ["pyplot"], mempty) (unpack script)

addCaption :: String -> Block -> Block
addCaption caption (CodeBlock (id', cls, attrs) script) = 
    CodeBlock (id', cls, attrs ++ [(Filter.captionKey, caption)]) script

addDirectory :: FilePath -> Block -> Block
addDirectory dir (CodeBlock (id', cls, attrs) script) = 
    CodeBlock (id', cls, attrs ++ [(Filter.directoryKey, dir)]) script

addInclusion :: FilePath -> Block -> Block
addInclusion inclusionPath (CodeBlock (id', cls, attrs) script) = 
    CodeBlock (id', cls, attrs ++ [(Filter.includePathKey, inclusionPath)]) script

addDPI :: Int -> Block -> Block
addDPI dpi (CodeBlock (id', cls, attrs) script) = 
    CodeBlock (id', cls, attrs ++ [(Filter.dpiKey, show dpi)]) script

-- | Assert that a file exists
assertFileExists :: HasCallStack 
                 => FilePath
                 -> Assertion
assertFileExists filepath = do
    fileExists <- doesFileExist filepath 
    unless fileExists (assertFailure msg)
    where
        msg = mconcat ["File ", filepath, " does not exist."]

-- | Assert that the first list is contained, 
-- wholly and intact, anywhere within the second.
assertIsInfix :: (Eq a, Show a, HasCallStack)
              => [a]
              -> [a]
              -> Assertion
assertIsInfix xs ys = 
    unless (xs `isInfixOf` ys) (assertFailure msg)
    where
        msg = mconcat ["Expected ", show xs, " to be an infix of ", show ys]

-------------------------------------------------------------------------------
-- Test that plot files and source files are created when the filter is run
testFileCreation :: TestTree
testFileCreation = testCase "writes output files in appropriate directory" $ do
    tempDir <- (</> "test-file-creation") <$> getCanonicalTemporaryDirectory
    exists <- doesDirectoryExist tempDir

    if exists 
        then removePathForcibly tempDir
        else return ()

    createDirectory tempDir

    let codeBlock = ( addDirectory tempDir  
                    $ plotCodeBlock "import matplotlib.pyplot as plt\n"
                    )    
    _ <- Filter.makePlot' codeBlock
    filesCreated <- length <$> listDirectory tempDir
    assertEqual "" filesCreated 3

-------------------------------------------------------------------------------
-- Test that included files are found within the source
testFileInclusion :: TestTree
testFileInclusion = testCase "includes plot inclusions" $ do
    tempDir <- (</> "test-file-inclusion") <$> getCanonicalTemporaryDirectory
    exists <- doesDirectoryExist tempDir

    if exists 
        then removePathForcibly tempDir
        else return ()

    createDirectory tempDir

    let codeBlock = ( addInclusion "test/fixtures/include.py"
                    $ addDirectory tempDir
                    $ plotCodeBlock "import matplotlib.pyplot as plt\n"
                    )
    _ <- Filter.makePlot' codeBlock

    inclusion <- readFile "test/fixtures/include.py"
    sourcePath <- head . filter (isExtensionOf "txt") <$> listDirectory tempDir
    src <- readFile (tempDir </> sourcePath)
    assertIsInfix inclusion src

-------------------------------------------------------------------------------
-- Test that a script containing a blockign call to matplotlib.pyplot.show
-- returns the appropriate error
testBlockingCallError :: TestTree
testBlockingCallError = testCase "raises an exception for blocking calls" $ do
    tempDir <- getCanonicalTemporaryDirectory

    let codeBlock = plotCodeBlock "import matplotlib.pyplot as plt\nplt.show()"

    result <- Filter.makePlot' codeBlock
    case result of
        Right block -> assertFailure "did not catch the expected blocking call"
        Left  error -> 
            if error == Filter.BlockingCallError 
            then pure () 
            else assertFailure "did not catch the expected blocking call"

-------------------------------------------------------------------------------
