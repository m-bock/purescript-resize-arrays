module Test.BenchLib.Reporters.ChartJsHtml where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Ca
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.String (Pattern(..))
import Data.String as Str
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, multiline, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Foreign.Object as Obj
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.BenchLib (SuiteResults, codecSuiteResults)
import Unsafe.Coerce (unsafeCoerce)

type Opts =
  { colors :: Map String String
  }

codecMapAsObj :: forall v. JsonCodec v -> JsonCodec (Map String v)
codecMapAsObj c = CA.codec dec enc
  where
  dec :: Json -> Either _ (Map String v)
  dec json = do
    objJson <- CA.decode Ca.jobject json
    obj <- for objJson \k ->
      CA.decode c k
    pure (Map.fromFoldableWithIndex obj)
  
  enc :: Map String v -> Json
  enc mp =
    CA.encode Ca.jobject $ Obj.fromFoldableWithIndex $ map (CA.encode c) mp

defaultOpts :: Opts
defaultOpts =
  { colors: Map.empty
  }

logChartJsHtml :: (Opts -> Opts) -> SuiteResults -> Effect Unit
logChartJsHtml mkOpts suiteResults = do
  let opts = mkOpts defaultOpts
  template <- FS.readTextFile UTF8 "test/Test/BenchLib/Reporters/template.html"

  let regex = unsafeRegex "(/\\* config start \\*/)([\\s\\S]*)(/\\* config end \\*/)" noFlags

  let
    config =
      { colors: opts.colors
      , data: suiteResults
      }

  let jsonStr = Json.stringifyWithIndent 2 $ CA.encode codecConfig config

  let indentStr = "      "

  let replacement = "$1\n" <> (indent indentStr ("const config = " <> jsonStr)) <> "\n" <> indentStr <> "$3"

  let out = Regex.replace regex replacement template

  Console.log out

indent :: String -> String -> String
indent indentStr str = Str.split (Pattern pat) str # map (indentStr <> _) # Str.joinWith pat
  where
  pat = "\n"

type Config =
  { colors :: Map String String
  , data :: SuiteResults
  }

codecConfig :: JsonCodec Config
codecConfig = CAR.object "Config"
  { colors: codecMapAsObj CA.string
  , data: codecSuiteResults
  }

type Color = { name :: String, color :: String }

codecColor :: JsonCodec Color
codecColor = CAR.object "Color"
  { name: CA.string
  , color: CA.string
  }