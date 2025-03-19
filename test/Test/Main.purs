module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Bench (benchs)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Specs (spec)

main :: Effect Unit
main = do
  --benchs
  launchAff_ $ runSpec [ consoleReporter ] do
    spec
