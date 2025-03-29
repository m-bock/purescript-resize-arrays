module Test.Bench where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (stringifyWithIndent)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CACommon
import Data.Codec.Argonaut.Record as CARecord
import Data.DateTime.Instant (unInstant)
import Data.Foldable (all)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.List as List
import Data.List.Lazy (replicateM)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Number.Format as NumFmt
import Data.Ord (abs)
import Data.Profunctor (dimap)
import Data.ResizeArray as RA
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, for_, sequence, sum)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (range)
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Effect.Exception as Ex
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as FO
import Safe.Coerce (coerce)
import Test.BenchLib (bench, benchAff, benchGroup_, benchSuite, bench_, run)
import Test.BenchLib.Reporters.ChartJsHtml (reportChartJs, reportChartJs_)
import Test.BenchLib.Reporters.Json (reportJson_)
import Test.MutArray as MutArray

main :: Effect Unit
main = run $
  benchSuite "PureScript collections"
    ( \def -> def
        { sizes = [ 25_000, 50_000, 100_000 ]
        , count = 1000
        , reporters = def.reporters <>
            [ reportJson_
            , reportChartJs
                ( \def -> def
                    { colors = Map.fromFoldable 
                        [ 
                        ]

                    }
                )
            ]
        }
    )
    [ benchGroup_ "simple"
        [ bench "one" (\def -> def { prepare = \size -> "", finalize = \_ -> 's' })
            (\(x) -> 1)
        , bench_ "two"
            (\_ -> 's')
        ]

    , benchGroup_ "Delete first item"
        [ bench "RA.drop"
            ( \def -> def
                { prepare = RA.fromArray <<< range 1
                , finalize = RA.toArray
                }
            )
            (\xs -> RA.drop 1 xs)

        , bench "Array.drop"
            ( \def -> def
                { prepare = range 1
                }
            )
            (\xs -> Array.drop 1 xs)

        , bench "List.drop"
            ( \def -> def
                { prepare = range 1
                , finalize = List.toUnfoldable
                }
            )
            (\xs -> List.drop 1 xs)

        , benchAff "MutArray.shift"
            ( \def -> def
                { prepare = pure <<< MutArray.fromArray <<< range 1
                , finalize = pure <<< MutArray.toArray
                }
            )
            (\xs -> do
                -- MutArray.shift xs
                (MutArray.fromArray []) -- xs
            )
        ]

    

    --     -- , bench "MutArray.shift"
    --     --     ( \def -> def
    --     --         { count = 1000
    --     --         , prepare = \n -> MutArray.fromArray [ 8 ]
    --     --         , finalize = MutArray.toArray
    --     --         }
    --     --     )
    --     --     MutArray.shift

    --     ]
    -- , benchGroup "Delete last item" (\def -> def)
    --     [ bench "RA.dropEnd"
    --         ( \def -> def
    --             { prepare = \n -> pure $ RA.fromArray $ range 1 n
    --             , finalize = pure <<< List.toUnfoldable <<< RA.toList :: _ -> _ (Array _)
    --             }
    --         )
    --         (\(xs) -> RA.dropEnd 1 xs)

    --     , bench "Array.dropEnd"
    --         ( \def -> def
    --             { prepare = \n -> pure $ range 1 n
    --             , finalize = pure :: _ -> _
    --             }
    --         )
    --         (\xs -> Array.dropEnd 1 xs)

    --     , bench "List.dropEnd"
    --         ( \def -> def
    --             { prepare = \n -> pure $ range 1 n
    --             , finalize = pure <<< List.toUnfoldable :: _ -> _ (Array _)
    --             }
    --         )
    --         (\xs -> List.dropEnd 1 xs)
    --     ]
    ]

main2 :: Effect Unit
main2 = do

  let ns = [ 25_000, 50_000, 100_000 ]
  benchSuite2 "PureScript collections"
    [
      -- [ runBench2 "Delete first item" ns \n ->
      --     let
      --       arr :: Array Int
      --       arr = range 1 n
      --     in
      --       [ mkBench2 "RA.drop"
      --           { pre: let xs = RA.fromArray arr in \_ -> pure xs
      --           , run: \ys -> pure $ RA.drop 1 ys
      --           , post: pure <<< List.toUnfoldable <<< RA.toList
      --           }
      --       , mkBench2 "Array.drop"
      --           { pre: \_ -> pure arr
      --           , run: \ys -> pure $ Array.drop 1 ys
      --           , post: pure
      --           }
      --       , mkBench2 "List.drop"
      --           { pre: let xs = List.fromFoldable arr in \_ -> pure xs
      --           , run: \ys -> pure $ List.drop 1 ys
      --           , post: pure <<< List.toUnfoldable
      --           }
      --       , mkBench2 "MutAttay.shift"
      --           { pre: \_ -> MutArray.fromArray arr
      --           , run: \ys -> do
      --               MutArray.shift ys
      --               pure ys
      --           , post: MutArray.toArray
      --           }
      --       ]
      -- , runBench2 "Delete last item" ns \n ->

      --     let
      --       arr :: Array Int
      --       arr = range 1 n
      --     in
      --       [ mkBench2 "RA.dropEnd"
      --           { pre: let xs = RA.fromArray arr in \_ -> pure xs
      --           , run: \ys -> pure $ RA.dropEnd 1 ys
      --           , post: pure <<< List.toUnfoldable <<< RA.toList
      --           }
      --       , mkBench2 "Array.dropEnd"
      --           { pre: \_ -> pure arr
      --           , run: \ys -> pure $ Array.dropEnd 1 ys
      --           , post: pure
      --           }
      --       , mkBench2 "List.dropEnd"
      --           { pre: let xs = List.fromFoldable arr in \_ -> pure xs
      --           , run: \ys -> pure $ List.dropEnd 1 ys
      --           , post: pure <<< List.toUnfoldable
      --           }
      --       , mkBench2 "MutArray.pop"
      --           { pre: \_ -> MutArray.fromArray arr
      --           , run: \ys -> do
      --               MutArray.pop ys
      --               pure ys
      --           , post: MutArray.toArray
      --           }
      --       ]
      runBench2 "Add item to start"
        ns
        \n ->
          let
            arr :: Array Int
            arr = range 1 n
          in
            [ mkBench2 "RA.cons"
                { pre: let xs = RA.fromArray arr in \_ -> pure xs
                , run: \ys -> pure $ RA.cons 0 ys
                , post: pure <<< List.toUnfoldable <<< RA.toList
                }
            , mkBench2 "Array.cons"
                { pre: \_ -> pure arr
                , run: \ys -> pure $ Array.cons 0 ys
                , post: pure
                }
            , mkBench2 "List.cons"
                { pre: let xs = List.fromFoldable arr in \_ -> pure xs
                , run: \ys -> pure $ List.Cons 0 ys
                , post: pure <<< List.toUnfoldable
                }
            , mkBench2 "MutArray.unshift"
                { pre: \_ -> MutArray.fromArray arr
                , run: \ys -> do
                    MutArray.unshift 0 ys
                    pure ys
                , post: MutArray.toArray
                }
            ]
    , runBench2 "Add item to end" ns \n ->
        let
          arr :: Array Int
          arr = range 1 n
        in
          [ mkBench2 "RA.snoc"
              { pre: let xs = RA.fromArray arr in \_ -> pure xs
              , run: \ys -> pure $ RA.snoc ys 0
              , post: pure <<< List.toUnfoldable <<< RA.toList
              }
          , mkBench2 "Array.snoc"
              { pre: \_ -> pure arr
              , run: \ys -> pure $ Array.snoc ys 0
              , post: pure
              }
          , mkBench2 "List.snoc"
              { pre: let xs = List.fromFoldable arr in \_ -> pure xs
              , run: \ys -> pure $ List.snoc ys 0
              , post: pure <<< List.toUnfoldable
              }
          , mkBench2 "MutArray.push"
              { pre: \_ -> MutArray.fromArray arr
              , run: \ys -> do
                  MutArray.push 0 ys
                  pure ys
              , post: MutArray.toArray
              }
          ]

    -- , runBench2 "Delete last item" ns \n ->
    --     let
    --       arr :: Array Char
    --       arr = replicate n 'a'
    --     in
    --       [ mkBench2 "RA.dropEnd"
    --           { pre: let xs = RA.fromArray arr in pure xs
    --           , run: \ys -> pure $ RA.dropEnd 1 ys
    --           , post: pure <<< List.toUnfoldable <<< RA.toList
    --           }
    --       , mkBench2 "Array.dropEnd"
    --           { pre: pure arr
    --           , run: \ys -> pure $ Array.dropEnd 1 ys
    --           , post: pure
    --           }
    --       , mkBench2 "List.dropEnd"
    --           { pre: let xs = List.fromFoldable arr in pure xs
    --           , run: \ys -> pure $ List.dropEnd 1 ys
    --           , post: pure <<< List.toUnfoldable
    --           }
    --       , mkBench2 "MutArray.pop"
    --           { pre: MutArray.fromArray arr
    --           , run: \ys -> do
    --               MutArray.pop ys
    --               pure ys
    --           , post: MutArray.toArray
    --           }
    --       ]
    -- , runBench
    --     "Delete first item"
    --     ns
    --     \n ->
    --       let
    --         arr :: Array Char
    --         arr = replicate n 'a'
    --       in
    --         [ mkBench "RA.drop" \_ ->
    --             let
    --               ys = RA.fromArray arr
    --             in
    --               \_ -> RA.drop 1 ys

    --         , mkBench "Array.drop" \_ ->
    --             let
    --               ys = arr
    --             in
    --               \_ -> Array.drop 1 ys

    --         , mkBench "List.drop" \_ ->
    --             let
    --               ys = List.fromFoldable arr
    --             in
    --               \_ -> List.drop 1 ys

    --         -- , mkBench "STArray.shift" \_ _ ->
    --         --     ST.run do
    --         --       ys <- STArray.unsafeThaw arr
    --         --       STArray.shift ys
    --         ]

    -- , runBench
    --     "Delete last item"
    --     ns
    --     \n ->
    --       let
    --         arr :: Array Char
    --         arr = replicate n 'a'
    --       in
    --         [ mkBench "RA.dropEnd" \_ ->
    --             let
    --               ys = RA.fromArray arr
    --             in
    --               \_ -> RA.dropEnd 1 ys

    --         , mkBench "Array.dropEnd" \_ ->
    --             let
    --               ys = arr
    --             in
    --               \_ -> Array.dropEnd 0 ys

    --         , mkBench "List.dropEnd" \_ ->
    --             let
    --               ys = List.fromFoldable arr
    --             in
    --               \_ -> List.dropEnd 1 ys

    --         , mkBench "STArray.pop" \_ _ ->
    --             ST.run do
    --               ys <- STArray.unsafeThaw arr
    --               STArray.pop ys
    --         ]
    -- , runBench "Add item to start"
    --     ns
    --     \n ->
    --       let
    --         arr :: Array Char
    --         arr = replicate n 'a'
    --       in
    --         [ mkBench "RA.cons" \_ ->
    --             let
    --               ys = RA.fromArray arr
    --             in
    --               \_ -> RA.cons 'b' ys

    --         , mkBench "Array.cons" \_ ->
    --             let
    --               ys = arr
    --             in
    --               \_ -> Array.cons 'b' ys

    --         , mkBench "List.cons" \_ ->
    --             let
    --               ys = List.fromFoldable arr
    --             in
    --               \_ -> List.Cons 'b' ys

    --         , mkBench "STArray.unshift" \_ _ ->
    --             ST.run do
    --               ys <- STArray.unsafeThaw arr
    --               STArray.unshift 'b' ys
    --         ]
    -- , runBench "map over items"
    --     ns
    --     \n ->
    --       let
    --         arr :: Array Char
    --         arr = replicate n 'a'
    --         f = \_ -> 'b'
    --       in
    --         [ mkBench "map (RA)" \_ ->
    --             let
    --               ys = RA.fromArray arr
    --             in
    --               \_ -> map f ys

    --         , mkBench "map (Array)" \_ ->
    --             let
    --               ys = arr
    --             in
    --               \_ -> map f ys

    --         , mkBench "map (List)" \_ ->
    --             let
    --               ys = List.fromFoldable arr
    --             in
    --               \_ -> map f ys
    --         , mkBench "map (STArray)" \_ _ ->
    --             ST.run do
    --               ys <- STArray.unsafeThaw arr
    --               ys' <- STArray.unsafeFreeze ys
    --               pure $ STArray.unsafeThaw $ map f (ys')
    --         ]
    -- , runBench "foldl over items"
    --     ns
    --     \n ->
    --       let
    --         arr :: Array Int
    --         arr = replicate n 0
    --         f = (+) --\acc _ -> acc
    --         init = 0
    --       in
    --         [ mkBench "foldl (RA)" \_ ->
    --             let
    --               ys = RA.fromArray arr
    --             in
    --               \_ -> foldl f init ys

    --         , mkBench "foldl (Array)" \_ ->
    --             let
    --               ys = arr
    --             in
    --               \_ -> foldl f init ys

    --         , mkBench "foldl (List)" \_ ->
    --             let
    --               ys = List.fromFoldable arr
    --             in
    --               \_ -> foldl f init ys

    --         , mkBench "foldl (STArray)" \_ _ ->
    --             ST.run do
    --               ys <- STArray.unsafeThaw arr
    --               ys' <- STArray.unsafeFreeze ys
    --               pure $ foldl f init ys'
    --         ]

    -- , runBench "foldr over items"
    --     ns
    --     \n ->
    --       let
    --         arr :: Array Int
    --         arr = replicate n 0
    --         f = (+) --\_ acc -> acc
    --         init = 0
    --       in
    --         [ mkBench "foldr (RA)" \_ ->
    --             let
    --               ys = RA.fromArray arr
    --             in
    --               \_ -> foldr f init ys

    --         , mkBench "foldr (Array)" \_ ->
    --             let
    --               ys = arr
    --             in
    --               \_ -> foldr f init ys

    --         , mkBench "foldr (List)" \_ ->
    --             let
    --               ys = List.fromFoldable arr
    --             in
    --               \_ -> foldr f init ys

    --         , mkBench "foldr (STArray)" \_ _ ->
    --             ST.run do
    --               ys <- STArray.unsafeThaw arr
    --               ys' <- STArray.unsafeFreeze ys
    --               pure $ foldr f init ys'
    --         ]

    -- , runBench "item lookup" ns \n ->
    --     let
    --       arr :: Array Char
    --       arr = replicate n 'a'
    --       index = n `div` 2
    --     in
    --       [ mkBench "RA.index" \_ ->
    --           let
    --             ys = RA.fromArray arr
    --           in
    --             \_ -> RA.index ys index

    --       , mkBench "Array.index" \_ ->
    --           let
    --             ys = arr
    --           in
    --             \_ -> Array.index ys index

    --       , mkBench "List.index" \_ ->
    --           let
    --             ys = List.fromFoldable arr
    --           in
    --             \_ -> List.index ys index

    --       , mkBench "STArray.peek" \_ _ ->
    --           ST.run do
    --             ys <- STArray.unsafeThaw arr
    --             STArray.peek index ys
    --       ]
    -- , runBench "size" ns \n ->
    --     let
    --       arr :: Array Char
    --       arr = replicate n 'a'
    --     in
    --       [ mkBench "RA.length" \_ ->
    --           let
    --             ys = RA.fromArray arr
    --           in
    --             \_ -> RA.length ys

    --       , mkBench "Array.length" \_ ->
    --           let
    --             ys = arr
    --           in
    --             \_ -> Array.length ys

    --       , mkBench "List.length" \_ ->
    --           let
    --             ys = List.fromFoldable arr
    --           in
    --             \_ -> List.length ys

    --       , mkBench "STArray.length" \_ _ ->
    --           ST.run do
    --             ys <- STArray.unsafeThaw arr
    --             STArray.length ys
    --       ]
    ]

type Stats =
  { name :: String
  , results :: Map String (Map Int Milliseconds)
  }

codecStats :: JsonCodec Stats
codecStats = CARecord.object "Stats"
  { name: CA.string
  , results: CACommon.map CA.string (CACommon.map CA.int codecMilliseconds)
  }

codecMilliseconds :: JsonCodec Milliseconds
codecMilliseconds = dimap unwrap wrap CA.number

benchSuite2 :: String -> Array (Effect Stats) -> Effect Unit
benchSuite2 groupName benchmarks = do
  allStats <- sequence benchmarks
  log $ stringifyWithIndent 2 $ CA.encode (CA.array codecStats) allStats

runBench :: forall a. String -> Array Int -> (Int -> Array (Effect (String /\ Milliseconds))) -> Effect Stats
runBench groupName ns mkBenchmarks = do
  refStats :: Ref (Object (Object String)) <- Ref.new FO.empty

  refStats2 :: Ref (Map String (Map Int Milliseconds)) <- Ref.new Map.empty

  Console.error groupName

  for_ ns \n -> do

    let benchmarks = mkBenchmarks n
    results <- for benchmarks \benchmark -> do
      benchmark
    -- let resultsSorted = Array.sortBy (compare `on` snd) results
    for_ results \(name /\ duration) -> do

      Ref.modify_
        ( FO.alter
            ( case _ of
                Nothing -> Just (FO.singleton (show n) (prettyPrint duration))
                Just stats -> Just (FO.insert (show n) (prettyPrint duration) stats)

            )
            name
        )
        refStats

      Ref.modify_
        (Map.insertWith Map.union name (Map.singleton n duration))
        refStats2

  stats <- Ref.read refStats2

  pure
    { name: groupName
    , results: stats
    }

runBench2 :: forall a. Eq a => Show a => String -> Array Int -> (Int -> Array (Effect (BenchResult a))) -> Effect Stats
runBench2 groupName ns mkBenchmarks = do
  refStats :: Ref (Map String (Map Int Milliseconds)) <- Ref.new Map.empty

  Console.error groupName

  for_ ns \n -> do

    Console.error ("  n = " <> show n)

    let benchmarks = mkBenchmarks n
    outputs <- for benchmarks \mkResult -> do
      { name, output, duration } <- mkResult

      Ref.modify_
        (Map.insertWith Map.union name (Map.singleton n duration))
        refStats

      pure output

    case (Array.head outputs) of
      Just output -> do
        unless (all (_ == output) outputs) do
          throwError (Ex.error $ "Results differ in group for n = " <> show n <> show outputs)
      Nothing -> pure unit

  stats <- Ref.read refStats

  pure
    { name: groupName
    , results: stats
    }

mapToObj :: Map String (Map Int Milliseconds) -> Object (Object String)
mapToObj =
  foldlWithIndex
    ( \name fo mp ->
        let
          complexity = getComplexity2 (List.toUnfoldable $ Map.values mp)
          lineColor = complexityToColor complexity
        in
          FO.insert name
            ( foldlWithIndex
                ( \n fo' ms ->
                    FO.insert (show n) (prettyPrint ms) fo'
                )
                FO.empty
                mp
                # FO.insert "Complexity" (printComplexity complexity)
            )
            fo
    )
    FO.empty

color :: Int -> String -> String
color c s = "\x1b[" <> show c <> "m" <> s <> "\x1b[0m"

complexityToColor :: Maybe Complexity -> Int
complexityToColor c = case c of
  Just Constant -> 32
  Just Linear -> 33
  Just Quadratic -> 31
  Nothing -> 0

prettyPrint :: Milliseconds -> String
prettyPrint (Milliseconds ms) = NumFmt.toStringWith (NumFmt.fixed 4) ms <> " ms"

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

type BenchResult a =
  { name :: String
  , output :: a
  , duration :: Milliseconds
  }

measureTime :: forall a. Effect a -> Effect (a /\ Milliseconds)
measureTime action = do
  startTime <- now
  result <- action
  endTime <- now
  let duration = unwrap (unInstant endTime) - unwrap (unInstant startTime)
  pure (result /\ Milliseconds duration)

calcMean :: NonEmptyList Milliseconds -> Milliseconds
calcMean items = Milliseconds (sum (map coerce items :: NonEmptyList Number) / Int.toNumber (NEL.length items))

-- calcMedian :: NonEmptyArray Milliseconds -> Milliseconds
-- calcMedian items = case Array.uncons (Array.sort (coerce items :: Array Number)) of
--   Just { head, tail } -> case Array.uncons tail of
--     Just { head: tail } -> Milliseconds head
--     _ -> Milliseconds head
--   _ -> Milliseconds 0.0

mkBench2 :: forall a b c. Eq c => String -> { pre :: Unit -> Effect a, run :: a -> Effect b, post :: b -> Effect c } -> Effect (BenchResult c)
mkBench2 name opts = do

  let n = 1_000

  input <- opts.pre unit

  durs :: NonEmptyList _ <- replicate1A n do

    -- (result /\ duration) <- measureTime do
    --   opts.run input

    input <- opts.pre unit

    startTime <- now
    _ <- opts.run input
    endTime <- now
    let duration = Milliseconds (unwrap (unInstant endTime) - unwrap (unInstant startTime))

    -- output <- opts.post result

    pure duration

  let mean = calcMean durs

  result <- opts.run input

  output <- opts.post result

  -- unless (all (_ == firstRes) results') do
  --   throwError (Ex.error "Results differ in bench")

  pure
    { name
    , output
    , duration: mean
    }

data Complexity = Constant | Linear | Quadratic

derive instance Eq Complexity

printComplexity :: Maybe Complexity -> String
printComplexity c = case c of
  Just Constant -> "O(1)"
  Just Linear -> "O(n)"
  Just Quadratic -> "O(n^2)"
  Nothing -> "?"

getComplexity2 :: Array Milliseconds -> Maybe Complexity
getComplexity2 ns = allSame (getComplexity' ns)

allSame :: forall a. Eq a => Array a -> Maybe a
allSame arr =
  case Array.uncons arr of
    Just { head, tail } | all (_ == head) tail -> Just head
    _ -> Nothing

getComplexity' :: Array Milliseconds -> Array Complexity
getComplexity' ns = case ns of
  [ n1, n2 ] -> case getComplexity n1 n2 of
    Just c -> [ c ] <> getComplexity' (Array.drop 1 ns)
    Nothing -> []
  _ -> []

getComplexity :: Milliseconds -> Milliseconds -> Maybe Complexity
getComplexity (Milliseconds n1) (Milliseconds n2) = case unit of
  _ | isAlmost n1 n2 -> Just Constant
  _ | isAlmost (n1 * 2.0) n2 -> Just Linear
  _ | isAlmost (n1 * n1) n2 -> Just Quadratic
  _ -> Nothing

isAlmost :: Number -> Number -> Boolean
isAlmost n1 n2 = diff n1 n2 < 0.01

diff :: Number -> Number -> Number
diff n1 n2 = abs (n1 - n2)

