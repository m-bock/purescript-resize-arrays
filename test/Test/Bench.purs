module Test.Bench (main) where

import Prelude

import BenchLib (bench, benchAff, group, suite)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml)
import Data.Array as Array
import Data.Foldable (all, foldl, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.ResizeArray as ResizeArray
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (range)
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.MutArray as MutArray

reporters :: Array BenchLib.Reporter
reporters =
  [ reportHtml
      ( \def -> def
          { filePath = "docs/benchmarks.html"
          , lineStyles =
              mapWithIndex (\ix val -> val { opacity = if ix == 0 then 1.0 else 0.3 }) def.lineStyles
          , minSpeed = Just $ Milliseconds 1.0
          }
      )
  ]

main :: Effect Unit
main =
  BenchLib.runNode
    (\cfg -> cfg { reporters = reporters <> cfg.reporters })
    $
      suite "PureScript collections Benchmarks"
        ( \def -> def
            { sizes = [ 0, 25_000, 50_000, 75_000, 100_000 ]
            , iterations = 100
            }
        )
        [ group "Delete first item"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 1 size) && (output == Array.drop 1 input)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.drop"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = ResizeArray.toArray
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 1 size
                , run: \xs -> ResizeArray.drop 1 xs
                }
            , bench
                "Array.drop"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> Array.drop 1 xs
                }
            , bench
                "List.drop"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = List.toUnfoldable
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> List.drop 1 xs
                }
            , benchAff
                "MutArray.shift"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = liftEffect <<< MutArray.toArray
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 1 size
                , run: \xs -> liftEffect do
                    MutArray.shift xs
                    pure xs
                }
            ]

        , group "Delete last item"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 1 size) && (output == Array.dropEnd 1 input)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.dropEnd"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = ResizeArray.toArray
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 1 size
                , run: \xs -> ResizeArray.dropEnd 1 xs
                }
            , bench
                "Array.dropEnd"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> Array.dropEnd 1 xs
                }
            , bench
                "List.dropEnd"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = List.toUnfoldable
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> List.dropEnd 1 xs
                }
            , benchAff
                "MutArray.pop"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = liftEffect <<< MutArray.toArray
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 1 size
                , run: \xs -> liftEffect do
                    MutArray.pop xs
                    pure xs
                }
            ]

        , group "Add item to start"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 1 size) && (output == Array.cons 0 input)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.cons"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = ResizeArray.toArray
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 1 size
                , run: \xs -> ResizeArray.cons 0 xs
                }
            , bench
                "Array.cons"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> Array.cons 0 xs
                }
            , bench
                "List.cons"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = List.toUnfoldable
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> List.Cons 0 xs
                }
            , benchAff
                "MutArray.unshift"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = liftEffect <<< MutArray.toArray
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 1 size
                , run: \xs -> liftEffect do
                    MutArray.unshift 0 xs
                    pure xs
                }
            ]

        , group "Add item to end"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 1 size) && (output == Array.snoc input 0)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.snoc"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = ResizeArray.toArray
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 1 size
                , run: \xs -> ResizeArray.snoc xs 0
                }
            , bench
                "Array.snoc"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> Array.snoc xs 0
                }
            , bench
                "List.snoc"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = List.toUnfoldable
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> List.snoc xs 0
                }
            , benchAff
                "MutArray.push"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = liftEffect <<< MutArray.toArray
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 1 size
                , run: \xs -> liftEffect do
                    MutArray.push 0 xs
                    pure xs
                }
            ]

        , group "map over items"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 1 size) && (output == map (_ + 1) input)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.map"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = ResizeArray.toArray
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 1 size
                , run: \xs -> map (_ + 1) xs
                }
            , bench
                "Array.map"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> map (_ + 1) xs
                }
            , bench
                "List.map"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = List.toUnfoldable
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> map (_ + 1) xs
                }
            , benchAff
                "MutArray.map"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = liftEffect <<< MutArray.toArray
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 1 size
                , run: \xs -> liftEffect do
                    xs' <- MutArray.map (_ + 1) xs
                    pure xs'
                }
            ]

        , group "foldl over items"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 0 size) && (output == foldl (+) 0 input)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.foldl"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = identity
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 0 size
                , run: \xs -> foldl (+) 0 xs
                }
            , bench
                "Array.foldl"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 0 size
                , run: \xs -> foldl @Array (+) 0 xs
                }
            , bench
                "List.foldl"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 0 size
                , run: \xs -> foldl @List (+) 0 xs
                }
            , benchAff
                "MutArray.foldl"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = pure
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 0 size
                , run: \xs -> liftEffect do
                    xs' <- MutArray.foldl (+) 0 xs
                    pure xs'
                }
            ]

        , group "foldr over items"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 0 size) && (output == foldr (+) 0 input)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.foldr"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = identity
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 0 size
                , run: \xs -> foldr (+) 0 xs
                }
            , bench
                "Array.foldr"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 0 size
                , run: \xs -> foldr @Array (+) 0 xs
                }
            , bench
                "List.foldr"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 0 size
                , run: \xs -> foldr @List (+) 0 xs
                }
            , benchAff
                "MutArray.foldr"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = pure
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 0 size
                , run: \xs -> liftEffect do
                    xs' <- MutArray.foldr (+) 0 xs
                    pure xs'
                }
            ]

        , group "item lookup"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == { index: size / 2, items: Array.range 1 size }) &&
                          (output == Array.index input.items input.index)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.index"
                ( \opts -> opts
                    { normIn = \{ index, items } -> { index, items: ResizeArray.toArray items }
                    , normOut = identity
                    }
                )
                { prepare: \size -> { index: size / 2, items: ResizeArray.fromArray $ range 1 size }
                , run: \{ index, items } -> ResizeArray.index items index
                }
            , bench
                "Array.index"
                ( \opts -> opts
                    { normIn = \{ index, items } -> { index, items }
                    , normOut = identity
                    }
                )
                { prepare: \size -> { index: size / 2, items: range 1 size }
                , run: \{ index, items } -> Array.index items index
                }
            , bench
                "List.index"
                ( \opts -> opts
                    { normIn = \{ index, items } -> { index, items: List.toUnfoldable items }
                    , normOut = identity
                    }
                )
                { prepare: \size -> { index: size / 2, items: range 1 size }
                , run: \{ index, items } -> List.index items index
                }
            , benchAff
                "MutArray.lookup"
                ( \opts -> opts
                    { normIn = \{ index, items: items_ } -> liftEffect do
                        items <- MutArray.toArray items_
                        pure { index, items }
                    , normOut = pure
                    }
                )
                { prepare: \size -> liftEffect do
                    items <- MutArray.fromArray $ range 1 size
                    pure { index: size / 2, items }

                , run: \{ index, items } -> liftEffect do
                    xs' <- MutArray.lookup index items
                    pure xs'
                }
            ]

        , group "size"
            ( \cfg -> cfg
                { check = Just \size -> all
                    ( \(input /\ output) ->
                        (input == Array.range 1 size) && (output == Array.length input)
                    )
                , printInput = Just show
                , printOutput = Just show
                }
            )
            [ bench
                "ResizeArray.length"
                ( \opts -> opts
                    { normIn = ResizeArray.toArray
                    , normOut = identity
                    }
                )
                { prepare: \size -> ResizeArray.fromArray $ range 1 size
                , run: \xs -> ResizeArray.length xs
                }
            , bench
                "Array.length"
                ( \opts -> opts
                    { normIn = identity
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> Array.length xs
                }
            , bench
                "List.length"
                ( \opts -> opts
                    { normIn = List.toUnfoldable
                    , normOut = identity
                    }
                )
                { prepare: \size -> range 1 size
                , run: \xs -> List.length xs
                }
            , benchAff
                "MutArray.length"
                ( \opts -> opts
                    { normIn = liftEffect <<< MutArray.toArray
                    , normOut = pure
                    }
                )
                { prepare: \size -> liftEffect $ MutArray.fromArray $ range 1 size
                , run: \xs -> liftEffect do
                    pure $ MutArray.length xs
                }
            ]
        ]
