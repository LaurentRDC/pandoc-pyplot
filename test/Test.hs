
module Test where

import Control.Monad     (unless)
import Data.List         (isInfixOf)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Pandoc.Filter.Pyplot as Filter
import qualified Text.Pandoc.Filter.Scripting as Filter
import Text.Pandoc.JSON

import System.Directory (doesFileExist)
import System.FilePath  ((</>))
import System.IO.Temp   (getCanonicalTemporaryDirectory)

-- | Create a code block with the right attributes to trigger pandoc-pyplot
mkPlotCodeBlock :: FilePath            -- ^ Plot target
                -> String              -- ^ Plot alt description
                -> String              -- ^ Plot caption
                -> Maybe FilePath      -- ^ Possible inclusion
                -> Filter.PythonScript -- ^ Script
                -> Block
mkPlotCodeBlock target alt caption include script = CodeBlock attrs script
    where
        attrs = case include of
            Nothing -> ( mempty , mempty
                       , [ ("plot_target", target)
                         , ("plot_alt", alt)
                         , ("plot_caption", caption)
                         ]
                       )
            Just includePath -> ( mempty , mempty
                                , [ ("plot_target", target)
                                , ("plot_alt", alt)
                                , ("plot_caption", caption)
                                , ("plot_include", includePath)
                                  ]
                                )

-- | Assert that a file exists
assertFileExists :: HasCallStack 
                 => FilePath
                 -> Assertion
assertFileExists filepath = do
    fileExists <- doesFileExist filepath 
    unless fileExists (assertFailure msg)
    where
        msg = mconcat ["File ", filepath, " does not exist."]

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

-- Test that plot files and source files are created when the filter is run
testFileCreation :: TestTree
testFileCreation = testCase "writes output and source files" $ do
    tempDir <- getCanonicalTemporaryDirectory
    let codeBlock = mkPlotCodeBlock 
            (tempDir </> "test.png") 
            mempty 
            mempty 
            Nothing
            "import matplotlib.pyplot as plt\n"    
    _ <- Filter.makePlot' codeBlock
    assertFileExists (tempDir </> "test.png")
    assertFileExists (tempDir </> "test.txt")

-- Test that included files are found within the source
testFileInclusion :: TestTree
testFileInclusion = testCase "includes plot inclusions" $ do
    tempDir <- getCanonicalTemporaryDirectory
    
    let codeBlock = mkPlotCodeBlock 
            (tempDir </> "test.png") 
            mempty 
            mempty 
            (Just "test/fixtures/include.py") 
            "import matplotlib.pyplot as plt\n"
    _ <- Filter.makePlot' codeBlock

    inclusion <- readFile "test/fixtures/include.py"
    src <- readFile (tempDir </> "test.txt")
    assertIsInfix inclusion src

-- Test that a script containing a blockign call to matplotlib.pyplot.show
-- returns the appropriate error
testBlockingCallError :: TestTree
testBlockingCallError = testCase "raises an exception for blocking calls" $ do
    tempDir <- getCanonicalTemporaryDirectory

    let codeBlock = mkPlotCodeBlock 
            (tempDir </> "test.png") 
            mempty 
            mempty 
            Nothing
            "import matplotlib.pyplot as plt\nplt.show()"
    
    result <- Filter.makePlot' codeBlock
    case result of
        Right block -> assertFailure "did not catch the expected blocking call"
        Left  error -> 
            if error == Filter.BlockingCallError 
            then pure () 
            else assertFailure "did not catch the expected blocking call"

tests = testGroup "Text.Pandoc.Filter.IncludeCode" 
    [ testFileCreation
    , testFileInclusion
    , testBlockingCallError
    ]