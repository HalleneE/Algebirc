module Main where

import Test.Hspec
import qualified Core.FiniteFieldSpec
import qualified Core.PolynomialSpec
import qualified StressSpec
import qualified AnalysisSpec
import qualified IntegrationSpec
import qualified NonlinearSpec
import qualified AdvancedAnalysisSpec
import qualified AdversarialSpec

main :: IO ()
main = hspec $ do
  Core.FiniteFieldSpec.spec
  Core.PolynomialSpec.spec
  StressSpec.spec
  AnalysisSpec.spec
  IntegrationSpec.spec
  NonlinearSpec.spec
  AdvancedAnalysisSpec.advancedAnalysisSpec
  AdversarialSpec.adversarialSpec
