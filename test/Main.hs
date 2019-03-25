{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad     (unless)

import Data.List         (isInfixOf)
import Data.Text         (unpack)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Pandoc.Filter.Pyplot as Filter
import qualified Text.Pandoc.Filter.Scripting as Filter
import Text.Pandoc.JSON

import System.Directory (doesFileExist, listDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath  ((</>))
import System.IO.Temp   (getCanonicalTemporaryDirectory)

main :: IO ()
main = defaultMain $ 
    testGroup "Text.Pandoc.Filter.Pyplot" 
        [ testFileCreationExplicitTarget
        , testFileCreationNoExplicitTarget
        , testFileInclusion
        , testBlockingCallError
        ]

plotCodeBlock :: Filter.PythonScript -> Block
plotCodeBlock script = CodeBlock (mempty, ["pyplot"], mempty) (unpack script)

addTarget :: FilePath -> Block -> Block
addTarget target (CodeBlock (id', cls, attrs) script) = 
    CodeBlock (id', cls, attrs ++ [(Filter.targetKey, target)]) script

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

-- | Assert that a directory is not empty
assertDirectoryNotEmpty :: HasCallStack 
                        => FilePath
                        -> Assertion
assertDirectoryNotEmpty filepath = do
    filesInDir <- (not . null) <$> listDirectory filepath 
    unless filesInDir (assertFailure msg)
    where
        msg = mconcat ["Directory ", filepath, " is empty."]

-- | Assert that a list first list is contained, 
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
testFileCreationExplicitTarget :: TestTree
testFileCreationExplicitTarget = testCase "writes output and source files" $ do
    tempDir <- getCanonicalTemporaryDirectory
    let codeBlock = ( addTarget (tempDir </> "test.png")  
                    $ plotCodeBlock "import matplotlib.pyplot as plt\n"
                    ) 
    _ <- Filter.makePlot' codeBlock
    assertFileExists (tempDir </> "test.png")
    assertFileExists (tempDir </> "test.hires.png")
    assertFileExists (tempDir </> "test.txt")

-------------------------------------------------------------------------------
-- Test that plot files and source files are created when the filter is run
testFileCreationNoExplicitTarget :: TestTree
testFileCreationNoExplicitTarget = testCase "writes output files in appropriate directory" $ do
    tempDir <- (</> "test-dir") <$> getCanonicalTemporaryDirectory
    createDirectoryIfMissing True tempDir
    let codeBlock = ( addDirectory tempDir  
                    $ plotCodeBlock "import matplotlib.pyplot as plt\n"
                    )    
    _ <- Filter.makePlot' codeBlock
    assertDirectoryNotEmpty tempDir
    removeDirectoryRecursive tempDir

-------------------------------------------------------------------------------
-- Test that included files are found within the source
testFileInclusion :: TestTree
testFileInclusion = testCase "includes plot inclusions" $ do
    tempDir <- getCanonicalTemporaryDirectory
    
    let codeBlock = ( addTarget (tempDir </> "test.png")
                    $ addInclusion "test/fixtures/include.py"
                    $ plotCodeBlock "import matplotlib.pyplot as plt\n"
                    )
    _ <- Filter.makePlot' codeBlock

    inclusion <- readFile "test/fixtures/include.py"
    src <- readFile (tempDir </> "test.txt")
    assertIsInfix inclusion src

-------------------------------------------------------------------------------
-- Test that a script containing a blockign call to matplotlib.pyplot.show
-- returns the appropriate error
testBlockingCallError :: TestTree
testBlockingCallError = testCase "raises an exception for blocking calls" $ do
    tempDir <- getCanonicalTemporaryDirectory

    let codeBlock = ( addTarget (tempDir </> "test.png")
                    $ plotCodeBlock "import matplotlib.pyplot as plt\nplt.show()"
                    )    
    result <- Filter.makePlot' codeBlock
    case result of
        Right block -> assertFailure "did not catch the expected blocking call"
        Left  error -> 
            if error == Filter.BlockingCallError 
            then pure () 
            else assertFailure "did not catch the expected blocking call"

-------------------------------------------------------------------------------
