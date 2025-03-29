module Test.BenchLib
  ( BenchResult
  , GroupResults
  , Reporter
  , Size
  , SuiteResults
  , bench
  , benchAff
  , benchAff_
  , benchGroup
  , benchGroup_
  , benchSuite
  , benchSuite_
  , bench_
  , defaultReporter
  , only
  , reportConsole
  , run
  ) where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, for_, sum)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Record as R
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

type Size = Int

type BenchName = String

type GroupName = String

type MayOnly a = { only :: Boolean, val :: a }

--- Opts

type SuiteOpts =
  { sizes :: Array Int
  , count :: Int
  , reporters :: Array Reporter
  }

type GroupOpts =
  { sizes :: Array Int
  , count :: Int
  , check :: forall a. Eq a => Array a -> Aff Unit
  , reporters :: Array Reporter
  }

type BenchOptsAff input result output =
  { count :: Int
  , prepare :: Size -> Aff input
  , finalize :: result -> Aff output
  , reporters :: Array Reporter
  }

type BenchOptsPure input result output =
  { count :: Int
  , prepare :: Size -> input
  , finalize :: result -> output
  , reporters :: Array Reporter
  }

--- Results

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

type Suite =
  { suiteName :: String
  , run :: SuiteOpts -> Aff SuiteResults
  }

type Group = MayOnly
  { groupName :: String
  , run :: GroupOpts -> Aff GroupResults
  }

type Bench out = MayOnly
  { benchName :: String
  , run :: BenchOptsPure Unit out out -> Size -> Aff (BenchResult /\ out)
  }

---

type Reporter =
  { onSuiteStart :: String -> Effect Unit
  , onGroupStart :: String -> Effect Unit
  , onSizeStart :: Size -> Effect Unit
  , onBenchStart :: String -> Effect Unit
  , onSuiteFinish :: SuiteResults -> Effect Unit
  , onGroupFinish :: GroupResults -> Effect Unit
  , onSizeFinish :: Size -> Effect Unit
  , onBenchFinish :: BenchResult -> Effect Unit
  }

run :: Suite -> Effect Unit
run suite = launchAff_ $ void $ suite.run defaultSuiteOpts

---

notOnly :: forall a. a -> MayOnly a
notOnly a = { only: false, val: a }

checkEq :: forall a. Eq a => Array a -> Aff Unit
checkEq items = unsafeCoerce 1

--- Defaults

defaultSuiteOpts :: SuiteOpts
defaultSuiteOpts =
  { sizes: [ 1, 10, 100, 1_000, 10_000 ]
  , count: 1000
  , reporters: [ reportConsole ]
  }

mkDefaultGroupOpts :: SuiteOpts -> GroupOpts
mkDefaultGroupOpts { sizes, count, reporters } =
  { sizes
  , count
  , check: checkEq
  , reporters
  }

mkDefaultBenchOpts :: forall c. GroupOpts -> BenchOptsPure Unit c c
mkDefaultBenchOpts { count, reporters } =
  { count
  , prepare: \_ -> unit
  , finalize: identity
  , reporters
  }

---
runReporters :: Array Reporter -> (Reporter -> Effect Unit) -> Aff Unit
runReporters reporters f = liftEffect $ for_ reporters f

---

benchSuite :: String -> (SuiteOpts -> SuiteOpts) -> Array Group -> Suite
benchSuite suiteName mkOpts groups_ =
  { suiteName
  , run: \_ -> do
      let opts = mkOpts defaultSuiteOpts

      let groups = mayGetOnlies groups_

      runReporters opts.reporters \rep -> rep.onSuiteStart suiteName

      groupResults <- for groups \group -> do
        --liftEffect $ Console.error group.groupName
        group.run (mkDefaultGroupOpts opts)

      let results = { suiteName, groups: groupResults }

      runReporters opts.reporters \rep -> rep.onSuiteFinish results

      pure results
  }

benchGroup :: forall @a. Eq a => Show a => String -> (GroupOpts -> GroupOpts) -> Array (Bench a) -> Group
benchGroup groupName mkOpts benches_ = notOnly
  { groupName
  , run: \defOpts -> do

      let groupOpts = mkOpts defOpts

      runReporters groupOpts.reporters \rep -> rep.onGroupStart groupName

      let benches = mayGetOnlies benches_

      results <- for groupOpts.sizes
        ( \size -> do

            runReporters groupOpts.reporters \rep -> rep.onSizeStart size

            resultsPerBench <- for benches
              ( \{ benchName, run } -> do
                  --Console.error (pad 10 (show size) <> " " <> benchName)

                  let benchOpts = mkDefaultBenchOpts groupOpts
                  { duration, count } /\ output <- run benchOpts size

                  pure ({ benchName, duration, count } /\ output)
              )

            runReporters groupOpts.reporters \rep -> rep.onSizeFinish size

            pure (map (\(r /\ _) -> R.merge r { size }) resultsPerBench)

        --checkResults resultsPerBench
        )

      let groupResults = { groupName, benchs: join results }

      runReporters groupOpts.reporters \rep -> rep.onGroupFinish groupResults

      pure groupResults
  }

benchImpl :: forall a b c. Eq c => String -> (BenchOptsPure Unit c c -> BenchOptsAff a b c) -> (a -> Aff b) -> Bench c
benchImpl benchName mkOpts benchFn = notOnly
  { benchName
  , run: \defOpts size -> do

      let opts@{ count } = mkOpts defOpts

      runReporters opts.reporters \rep -> rep.onBenchStart benchName

      durs :: NonEmptyList _ <- replicate1A count do

        -- (result /\ duration) <- measureTime do
        --   opts.run input

        input :: a <- opts.prepare size

        startTime <- liftEffect now
        _ <- benchFn input
        endTime <- liftEffect now
        let duration = Milliseconds (unwrap (unInstant endTime) - unwrap (unInstant startTime))

        pure duration

      let duration = calcMean durs

      let
        benchResult =
          { count
          , duration
          , size
          , benchName
          }

      runReporters opts.reporters \rep -> rep.onBenchFinish benchResult

      output <- do
        input :: a <- opts.prepare size
        result <- benchFn input
        opts.finalize result

      pure (benchResult /\ output)
  }

benchOptsPureToAff :: forall a b c. BenchOptsPure a b c -> BenchOptsAff a b c
benchOptsPureToAff { count, prepare, finalize, reporters } =
  { count
  , prepare: \size -> pure $ prepare size
  , finalize: \result -> pure $ finalize result
  , reporters
  }

bench :: forall a b c. Eq c => String -> (BenchOptsPure Unit c c -> BenchOptsPure a b c) -> (a -> b) -> Bench c
bench name mkOpts benchFn = benchImpl name (benchOptsPureToAff <<< mkOpts) (pure <<< benchFn)

bench_ :: forall c. Eq c => String -> (Unit -> c) -> Bench c
bench_ name benchFn = bench name identity benchFn

benchAff :: forall a b c. Eq c => String -> (BenchOptsAff Unit c c -> BenchOptsAff a b c) -> (a -> Aff b) -> Bench c
benchAff name mkOpts benchFn = benchImpl name (mkOpts <<< benchOptsPureToAff) benchFn

benchAff_ :: forall c. Eq c => String -> (Unit -> Aff c) -> Bench c
benchAff_ name benchFn = benchAff name identity benchFn

mayGetOnlies :: forall a. Array (MayOnly a) -> Array a
mayGetOnlies mayOnlies =
  let
    onlys = filter _.only mayOnlies
  in
    map _.val $ if Array.null onlys then mayOnlies else onlys

checkResults :: forall r a. Array { benchName :: String, output :: a | r } -> Effect Unit
checkResults _ = unsafeCoerce 1

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

--- Utils ---

calcMean :: NonEmptyList Milliseconds -> Milliseconds
calcMean items = Milliseconds (sum (map coerce items :: NonEmptyList Number) / Int.toNumber (NEL.length items))

---

benchSuite_ :: String -> Array Group -> Suite
benchSuite_ groupName benchmarks = benchSuite groupName identity benchmarks

benchGroup_ :: forall @a. Eq a => Show a => String -> Array (Bench a) -> Group
benchGroup_ groupName benches = benchGroup groupName (\opt -> opt { check = \_ -> pure unit }) benches

---

reportConsole :: Reporter
reportConsole = defaultReporter
  { onSuiteStart = \name -> Console.error ("running suite: " <> name)
  , onGroupStart = \name -> Console.error ("  running group: " <> name)
  , onSizeStart = \size -> Console.error ("    running size: " <> show size)
  , onBenchStart = \name -> Console.error ("      running bench: " <> name)
  }

defaultReporter :: Reporter
defaultReporter =
  { onSuiteStart: const $ pure unit
  , onGroupStart: const $ pure unit
  , onSizeStart: const $ pure unit
  , onBenchStart: const $ pure unit
  , onSuiteFinish: const $ pure unit
  , onGroupFinish: const $ pure unit
  , onSizeFinish: const $ pure unit
  , onBenchFinish: const $ pure unit
  }

