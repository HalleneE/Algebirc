module Main where

import Test.Hspec
import qualified PropertySpec
import qualified RichelotSpec
import qualified Core.ResultantSpec
import qualified Core.MatrixSpec
import qualified PipelineSpec

main :: IO ()
main = hspec $ do
  PropertySpec.spec
  RichelotSpec.spec
  Core.ResultantSpec.spec
  Core.MatrixSpec.spec
  PipelineSpec.spec
