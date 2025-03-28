module Test.BenchLib.Reporters.Json where

import Prelude



reportJson :: SuiteResults -> Effect Unit
reportJson results = Console.log $ toJsonStr results
