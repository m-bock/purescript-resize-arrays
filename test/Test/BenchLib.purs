module Test.BenchLib
  ( BenchResult
  , GroupResults
  , Size
  , SuiteResults
  , bench
  , benchEffect
  , benchEffectWith
  , benchGroup
  , benchGroupWith
  , benchSuite
  , benchSuiteWith
  , benchWith
  , codecSuiteResults
  , logJson
  , only
  )
  where

import Prelude

import Data.Argonaut (stringifyWithIndent)
import Data.Array (filter)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CACommon
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime.Instant (unInstant)
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, sum)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicate)
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Record as R
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

type Size = Int

type BenchName = String

type GroupName = String

type SuiteResults =
  { suiteName :: String
  , groups :: Array GroupResults
  }

type GroupResults =
  { groupName :: String
  , benchs :: Array BenchResult
  }

type BenchResult =
  { benchName :: String
  , size :: Size
  , duration :: Milliseconds
  , count :: Int
  }

---

type BenchOutput a =
  { output :: a
  , count :: Int
  , duration :: Milliseconds
  }

type Bench out = MayOnly
  { benchName :: String
  , run :: BenchOpts Unit out out -> Size -> Effect (BenchOutput out)
  }

type Group = MayOnly
  { groupName :: String
  , run :: GroupOpts -> Effect GroupResults
  }

type SuiteOpts =
  { sizes :: Array Int
  , count :: Int
  , logger :: SuiteResults -> Effect Unit
  }

type GroupOpts =
  { sizes :: Array Int
  , count :: Int
  , check :: forall a. Eq a => Array a -> Effect Unit
  }

type BenchOpts input result output =
  { count :: Int
  , prepare :: Size -> Effect input
  , finalize :: result -> Effect output
  }

type MayOnly a = { only :: Boolean, val :: a }

notOnly :: forall a. a -> MayOnly a
notOnly a = { only: false, val: a }

checkEq :: forall a. Eq a => Array a -> Effect Unit
checkEq items = unsafeCoerce 1

defaultBenchSuiteOpts :: SuiteOpts
defaultBenchSuiteOpts =
  { sizes: [ 1, 10, 100, 1_000, 10_000 ]
  , count: 1000
  , logger: logJson
  }

mkDefaultGroupOpts :: SuiteOpts -> GroupOpts
mkDefaultGroupOpts { sizes, count } =
  { sizes
  , count
  , check: checkEq
  }

mkDefaultBenchOpts :: forall c. GroupOpts -> BenchOpts Unit c c
mkDefaultBenchOpts { count } =
  { count
  , prepare: const (pure unit)
  , finalize: pure
  }

toJsonStr :: SuiteResults -> String
toJsonStr = stringifyWithIndent 2 <<< CA.encode codecSuiteResults

logJson :: SuiteResults -> Effect Unit
logJson results = Console.log $ toJsonStr results

benchSuite :: String -> Array Group -> Effect Unit
benchSuite groupName benchmarks = benchSuiteWith groupName identity benchmarks

benchSuiteWith :: String -> (SuiteOpts -> SuiteOpts) -> Array Group -> Effect Unit
benchSuiteWith suiteName mkOpts groups_ = do
  let opts = mkOpts defaultBenchSuiteOpts

  let groups = mayGetOnlies groups_

  groupResults <- for groups \group -> do
    Console.error group.groupName
    group.run (mkDefaultGroupOpts opts)

  let results = { suiteName, groups: groupResults }
  opts.logger results

benchGroup :: forall @a. Eq a => Show a => String -> Array (Bench a) -> Group
benchGroup groupName benches = benchGroupWith groupName (\{ sizes, count, check } -> { sizes, count, check: \_ -> pure unit }) benches

mayGetOnlies :: forall a. Array (MayOnly a) -> Array a
mayGetOnlies mayOnlies =
  let
    onlys = filter _.only mayOnlies
  in
    map _.val $ if Array.null onlys then mayOnlies else onlys

checkResults :: forall r a. Array { benchName :: String, output :: a | r } -> Effect Unit
checkResults _ = unsafeCoerce 1

benchGroupWith :: forall @a. Eq a => Show a => String -> (GroupOpts -> GroupOpts) -> Array (Bench a) -> Group
benchGroupWith groupName mkOpts benches_ = notOnly
  { groupName
  , run: \defOpts -> do

      let groupOpts = mkOpts defOpts
      let benchOpts = mkDefaultBenchOpts groupOpts

      let benches = mayGetOnlies benches_

      results <- for groupOpts.sizes
        ( \size -> do
            Console.error ("  ")

            resultsPerBench <- for benches
              ( \{ benchName, run } -> do
                  Console.error (pad 10 (show size) <> " " <> benchName)

                  { output, duration, count } <- run benchOpts size

                  pure ({ benchName, duration, count } /\ output)
              )

            pure (map (\(r /\ _) -> R.merge r { size }) resultsPerBench)

        --checkResults resultsPerBench
        )

      pure
        { groupName
        , benchs: join results
        }
  }

pad :: Int -> String -> String
pad n str = Str.joinWith "" (replicate (n - Str.length str) ".") <> str

only :: forall a. MayOnly a -> MayOnly a
only { val } = { val, only: true }

-- measureTime :: forall a. Effect a -> Effect (a /\ Milliseconds)
-- measureTime action = do
--   startTime <- now
--   result <- action
--   endTime <- now
--   let duration = unwrap (unInstant endTime) - unwrap (unInstant startTime)
--   pure (result /\ Milliseconds duration)

benchEffect :: String -> (Unit -> Effect Unit) -> Bench Unit
benchEffect name benchFn = benchEffectWith name identity benchFn

benchEffectWith :: forall a b c. Eq c => String -> (BenchOpts Unit c c -> BenchOpts a b c) -> (a -> Effect b) -> Bench c
benchEffectWith benchName mkOpts benchFn = notOnly
  { benchName
  , run: \defOpts size -> do

      let opts = mkOpts defOpts
      let count = opts.count

      durs :: NonEmptyList _ <- replicate1A count do

        -- (result /\ duration) <- measureTime do
        --   opts.run input

        input :: a <- opts.prepare size

        startTime <- now
        _ <- benchFn input
        endTime <- now
        let duration = Milliseconds (unwrap (unInstant endTime) - unwrap (unInstant startTime))

        pure duration

      let duration = calcMean durs

      input :: a <- opts.prepare size
      result <- benchFn input
      output <- opts.finalize result

      pure
        { count
        , output
        , duration
        }
  }

benchWith :: forall a b c. Eq c => String -> (BenchOpts Unit c c -> BenchOpts a b c) -> (a -> b) -> Bench c
benchWith name mkOpts benchFn = benchEffectWith name mkOpts (pure <<< benchFn)

bench :: forall x. Eq x => String -> (Unit -> x) -> Bench x
bench name benchFn = benchWith name identity (\x -> benchFn x)

--- Utils ---

calcMean :: NonEmptyList Milliseconds -> Milliseconds
calcMean items = Milliseconds (sum (map coerce items :: NonEmptyList Number) / Int.toNumber (NEL.length items))

--- Codecs ---

codecSuiteResults :: JsonCodec SuiteResults
codecSuiteResults = CAR.object "SuiteResults"
  { suiteName: CA.string
  , groups: CACommon.array codecGroupResults
  }

codecGroupResults :: JsonCodec GroupResults
codecGroupResults = CAR.object "GroupResults"
  { groupName: CA.string
  , benchs: CA.array codecBenchResult
  }

codecBenchResult :: JsonCodec BenchResult
codecBenchResult = CAR.object "BenchResult"
  { benchName: CA.string
  , size: CA.int
  , duration: codecMilliseconds
  , count: CA.int
  }

codecMilliseconds :: JsonCodec Milliseconds
codecMilliseconds = dimap unwrap wrap CA.number