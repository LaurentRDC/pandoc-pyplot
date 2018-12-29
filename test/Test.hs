
module Test where

import Control.Monad  (unless)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Pandoc.Filter.Pyplot as Filter
import qualified Text.Pandoc.Filter.Scripting as Filter
import Text.Pandoc.JSON

import System.Directory (doesFileExist)
import System.FilePath  ((</>))
import System.IO.Temp   (getCanonicalTemporaryDirectory)

-- | Create a code block with the right attributes to trigger pandoc-pyplot
mkPlotCodeBlock :: FilePath     -- ^ Plot target
                -> String       -- ^ Plot alt description
                -> String       -- ^ Plot caption
                -> Filter.PythonScript -- ^ Script
                -> Block
mkPlotCodeBlock target alt caption script =
    let attrs = ( mempty 
                , mempty
                , [ ("plot_target", target)
                  , ("plot_alt", alt)
                  , ("plot_caption", caption)
                  ]
                )
    in CodeBlock attrs script

-- | Assert that a file exists
assertFileExists :: HasCallStack 
                 => FilePath
                 -> Assertion
assertFileExists filepath = do
    fileExists <- doesFileExist filepath 
    unless fileExists (assertFailure msg)
    where
        msg = mconcat ["File ", filepath, " does not exist."]

testFileCreation :: TestTree
testFileCreation = testCase "writes output and source files" $ do
    tempDir <- getCanonicalTemporaryDirectory
    let codeBlock = mkPlotCodeBlock (tempDir </> "test.jpg") "" "" "import matplotlib.pyplot as plt\n"
    _ <- Filter.makePlot' codeBlock
    assertFileExists (tempDir </> "test.jpg")
    -- assertFileExists (tempDir </> "test.txt")

tests = testGroup "Text.Pandoc.Filter.IncludeCode" 
    [ testFileCreation
    ]