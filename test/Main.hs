module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test

main :: IO ()
main = defaultMain Test.tests