module Test.Bench where

import Prelude

import Data.Array (cons, deleteAt, dropEnd, index, length, snoc, sortBy) as Array
import Data.Array (replicate)
import Data.DateTime.Instant (unInstant)
import Data.ResizeArray as DLL
import Data.Foldable (foldr)
import Data.Function (on)
import Data.Int as Int
import Data.List as List
import Data.List.Lazy (replicateM)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Number.Format as NumFmt
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (foldl, for, for_, sum)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Effect.Now (now)
import Test.Spec (Spec, describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

benchs :: Effect Unit
benchs = do
  pure unit
  -- runBench "Delete first item"
  --   let
  --     arr :: Array Char
  --     arr = replicate 100_000 'a'
  --   in
  --     [ mkBench "DLL.deleteStart" \_ ->
  --         let
  --           ys = DLL.fromArray arr
  --         in
  --           \_ -> DLL.deleteStart ys

  --     , mkBench "Array.deleteAt" \_ ->
  --         let
  --           ys = arr
  --         in
  --           \_ -> Array.deleteAt 0 ys

  --     , mkBench "List.deleteAt" \_ ->
  --         let
  --           ys = List.fromFoldable arr
  --         in
  --           \_ -> List.deleteAt 0 ys
  --     ]

  -- runBench "Delete last item"
  --   let
  --     xs = replicate 100_000 'a'
  --   in
  --     [ mkBench "DLL.deleteEnd" \_ ->
  --         let
  --           ys = DLL.fromArray xs
  --         in
  --           \_ -> DLL.deleteEnd ys

  --     , mkBench "Array.deleteAt" \_ ->
  --         let
  --           ys = xs
  --         in
  --           \_ -> Array.dropEnd 1 ys

  --     , mkBench "List.deleteAt" \_ ->
  --         let
  --           ys = List.fromFoldable xs
  --         in
  --           \_ -> List.dropEnd 1 ys
  --     ]

  -- -- runBench "Add item to start"
  -- --   let
  -- --     arr = replicate 100_000 'a'
  -- --   in
  -- --     [ mkBench "DLL.addStart" \_ ->
  -- --         let
  -- --           target = DLL.fromArray arr
  -- --         in
  -- --           \_ -> DLL.addStart 'b' target

  -- --     , mkBench "Array.cons" \_ ->
  -- --         let
  -- --           target = arr
  -- --         in
  -- --           \_ -> Array.cons 'b' target

  -- --     , mkBench "List.cons" \_ ->
  -- --         let
  -- --           target = List.fromFoldable arr
  -- --         in
  -- --           \_ -> List.Cons 'b' target
  -- --     ]

  -- runBench "Add item to end"
  --   let
  --     arr = replicate 100_000 'a'
  --     item = 'b'
  --   in
  --     [ mkBench "DLL.addEnd" \_ ->
  --         let
  --           target = DLL.fromArray arr
  --         in
  --           \_ -> DLL.addEnd item target

  --     , mkBench "Array.snoc" \_ ->
  --         let
  --           target = arr
  --         in
  --           \_ -> Array.snoc target item

  --     , mkBench "List.snoc" \_ ->
  --         let
  --           target = List.fromFoldable arr
  --         in
  --           \_ -> List.snoc target item
  --     ]

  -- runBench "map over items"
  --   let
  --     arr = replicate 100_000 'a'
  --     f = \_ -> 'b'
  --   in
  --     [ mkBench "DLL.map" \_ ->
  --         let
  --           target = DLL.fromArray arr
  --         in
  --           \_ -> map f target

  --     , mkBench "Array.map" \_ ->
  --         let
  --           target = arr
  --         in
  --           \_ -> map f target

  --     , mkBench "List.map" \_ ->
  --         let
  --           target = List.fromFoldable arr
  --         in
  --           \_ -> map f target
  --     ]

  -- runBench "foldl over items"
  --   let
  --     arr = replicate 100_000 0
  --     f = \acc _ -> acc
  --     init = 0
  --   in
  --     [ mkBench "DLL.foldl" \_ ->
  --         let
  --           target = DLL.fromArray arr
  --         in
  --           \_ -> foldl f init target

  --     , mkBench "Array.foldl" \_ ->
  --         let
  --           target = arr
  --         in
  --           \_ -> foldl f init target

  --     , mkBench "List.foldl" \_ ->
  --         let
  --           target = List.fromFoldable arr
  --         in
  --           \_ -> foldl f init target

  --     ]

  -- runBench "foldr over items"
  --   let
  --     arr = replicate 100_000 0
  --     f = \_ acc -> acc
  --     init = 0
  --   in
  --     [ mkBench "DLL.foldr" \_ ->
  --         let
  --           target = DLL.fromArray arr
  --         in
  --           \_ -> foldr f init target

  --     , mkBench "Array.foldr" \_ ->
  --         let
  --           target = arr
  --         in
  --           \_ -> foldr f init target

  --     , mkBench "List.foldr" \_ ->
  --         let
  --           target = List.fromFoldable arr
  --         in
  --           \_ -> foldr f init target
  --     ]

  -- for_ [ 100_000, 200_000, 400_000 ] \n ->
  --   runBench "item lookup"
  --     let
  --       arr = replicate n 'a'
  --       index = n `div` 2
  --     in
  --       [ mkBench "DLL.lookup" \_ ->
  --           let
  --             target = DLL.fromArray arr
  --           in
  --             \_ -> DLL.lookup index target
  --       , mkBench "Array.lookup" \_ ->
  --           let
  --             target = arr
  --           in
  --             \_ -> Array.index target index
  --       , mkBench "List.lookup" \_ ->
  --           let
  --             target = List.fromFoldable arr
  --           in
  --             \_ -> List.index target index
  --       ]

  -- for_ [ 100_000, 200_000, 400_000 ] \n ->
  --   runBench "size"
  --     let
  --       arr = replicate n 'a'
  --     in
  --       [ mkBench "Map.size" \_ ->
  --           let
  --             target = Map.fromFoldableWithIndex arr
  --           in
  --             \_ -> Map.size target
  --       , mkBench "Array.length" \_ ->
  --           let
  --             target = arr
  --           in
  --             \_ -> Array.length target
  --       , mkBench "List.length" \_ ->
  --           let
  --             target = List.fromFoldable arr
  --           in
  --             \_ -> List.length target
  --       ]

runBench :: String -> Array (Effect (String /\ Milliseconds)) -> Effect Unit
runBench groupName benchmarks = do
  log $ "\nRunning group: " <> groupName
  results <- for benchmarks \benchmark -> do
    benchmark
  let resultsSorted = Array.sortBy (compare `on` snd) results
  for_ resultsSorted \(name /\ duration) -> do
    log (NumFmt.toStringWith (NumFmt.fixed 4) (unwrap duration) <> "ms " <> name)

type BenchOpts =
  { count :: Int
  }

mkBench :: forall a. String -> (Unit -> Unit -> a) -> Effect (String /\ Milliseconds)
mkBench name prepareBench = do
  let count = 1_000
  let bench = prepareBench unit
  results <- replicateM count do
    startTime <- now
    let _ = bench unit
    endTime <- now
    let duration = unwrap (unInstant endTime) - unwrap (unInstant startTime)
    pure duration

  let mean = Milliseconds (sum results / Int.toNumber count)

  pure (name /\ mean)
