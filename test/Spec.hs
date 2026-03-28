module Main where

import Test.Hspec
import qualified PropertySpec
import qualified RichelotSpec
import qualified Core.ResultantSpec
import qualified Core.MatrixSpec
import qualified Core.U256Spec
import qualified PipelineSpec
import qualified PureAlgebraicSpec
import qualified SideChannelSpec

main :: IO ()
main = hspec $ do
  PropertySpec.spec
  RichelotSpec.spec
  Core.ResultantSpec.spec
  Core.MatrixSpec.spec
  Core.U256Spec.spec
  PipelineSpec.spec
  PureAlgebraicSpec.spec
  SideChannelSpec.spec
