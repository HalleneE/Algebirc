module Main where

import Test.Hspec
import qualified PropertySpec
import qualified RichelotSpec

main :: IO ()
main = hspec $ do
  PropertySpec.spec
  RichelotSpec.spec
