module Test.Specs where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.ResizeArray (ResizeArray)
import Data.ResizeArray as RA
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = do
  describe "show" do
    it "empty ResizeArray" do
      show (RA.empty :: _ Char)
        `shouldEqual`
          "(fromArrayAt 0 [])"

    it "populated ResizeArray" do
      show (RA.fromArray [ 'a', 'b', 'c' ])
        `shouldEqual`
          "(fromArrayAt 0 ['a','b','c'])"

  describe "index" do
    it "does not find non existing index" do
      RA.index (RA.fromArrayAt 10 [ 'a', 'b', 'c' ]) 50
        `shouldEqual`
          Nothing

    it "finds existing index" do
      RA.index (RA.fromArrayAt 10 [ 'a', 'b', 'c' ]) 11
        `shouldEqual`
          Just 'b'

  describe "emptyAt" do
    it "creates empty ResizeArray at given index" do
      RA.debug2 (RA.emptyAt 50 :: _ Char)
        `shouldEqual`
          (49 /\ [])

  describe "fromArrayAt" do
    it "start index zero, empty array" do
      RA.debug2 (RA.fromArrayAt 0 [] :: _ Char)
        `shouldEqual`
          (-1 /\ [])

    it "start index zero, populated array" do
      RA.debug2 (RA.fromArrayAt 0 [ 'a', 'b', 'c' ])
        `shouldEqual`
          (-1 /\ [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ])

    it "start index non zero, empty array" do
      RA.debug2 (RA.fromArrayAt 10 [] :: _ Char)
        `shouldEqual`
          (9 /\ [])

    it "start index non zero, populated array" do
      RA.debug2 (RA.fromArrayAt 10 [ 'a', 'b', 'c' ])
        `shouldEqual`
          (9 /\ [ 10 /\ 'a', 11 /\ 'b', 12 /\ 'c' ])

  describe "eq" do
    it "empty ResizeArray" do
      (RA.empty :: _ Char)
        `shouldEqual`
          RA.empty

    it "ResizeArrays with equal start index and equal items are considered equal" do
      RA.fromArrayAt 0 [ 'a', 'b', 'c' ]
        `shouldEqual`
          RA.fromArrayAt 0 [ 'a', 'b', 'c' ]

    it "ResizeArrays different start index are considered inequal" do
      RA.fromArrayAt 0 [ 'a', 'b', 'c' ]
        `shouldNotEqual`
          RA.fromArrayAt 10 [ 'a', 'b', 'c' ]

    it "ResizeArrays with different items are considered inequal" do
      RA.fromArrayAt 0 [ 'a', 'b', 'c' ]
        `shouldNotEqual`
          RA.fromArrayAt 0 [ 'a', 'b', 'd' ]

  describe "map" do
    it "maps over empty ResizeArray" do
      map (\x -> x) (RA.empty :: _ Char)
        `shouldEqual`
          RA.empty

    it "maps over populated ResizeArray" do
      map (_ <> "!") (RA.fromArray [ "a", "b", "c" ])
        `shouldEqual`
          RA.fromArray [ "a!", "b!", "c!" ]

  describe "foldlWithIndex" do
    it "accumulates with index from left to right" do
      foldlWithIndex (\idx acc val -> acc <> [ idx /\ val ]) [] (RA.fromArray [ 'a', 'b', 'c' ])
        `shouldEqual`
          [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ]

  describe "foldrWithIndex" do
    it "accumulates with index from right to left" do
      foldrWithIndex (\idx val acc -> acc <> [ idx /\ val ]) [] (RA.fromArray [ 'a', 'b', 'c' ])
        `shouldEqual`
          [ 2 /\ 'c', 1 /\ 'b', 0 /\ 'a' ]

  describe "foldl" do
    it "accumulates from left to right" do
      foldl (\acc val -> acc <> [ val ]) [] (RA.fromArray [ 'a', 'b', 'c' ])
        `shouldEqual`
          [ 'a', 'b', 'c' ]

  ---

  describe "foldr" do
    it "accumulates from right to left" do
      foldr (\val acc -> acc <> [ val ]) [] (RA.fromArray [ 'a', 'b', 'c' ])
        `shouldEqual`
          [ 'c', 'b', 'a' ]

  describe "cons" do
    it "adds an item to the start of an empty ResizeArray" do
      RA.debug2 (RA.cons 'a' $ RA.empty :: _ Char)
        `shouldEqual`
          (-1 /\ [ 0 /\ 'a' ])

    it "adds an item to the start of a populated ResizeArray" do
      RA.debug2 (RA.cons 'a' $ RA.fromArray [ 'b', 'c', 'd' ])
        `shouldEqual`
          (-2 /\ [ -1 /\ 'a', 0 /\ 'b', 1 /\ 'c', 2 /\ 'd' ])

    it "adds an item to the start of a populated ResizeArray (wrapping indices)" do
      RA.debug2 (RA.cons 'a' $ RA.fromArrayAt bottom [ 'b', 'c', 'd' ])
        `shouldEqual`
          ((top - 1) /\ [ top /\ 'a', bottom /\ 'b', (bottom + 1) /\ 'c', (bottom + 2) /\ 'd' ])

  describe "snoc" do
    it "adds an item to the end of an empty ResizeArray" do
      RA.debug2 (RA.snoc (RA.empty :: _ Char) 'a')
        `shouldEqual`
          (-1 /\ [ 0 /\ 'a' ])

    it "adds an item to the end of a populated ResizeArray" do
      RA.debug2 (RA.snoc (RA.fromArray [ 'a', 'b', 'c' ]) 'd')
        `shouldEqual`
          (-1 /\ [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c', 3 /\ 'd' ])

    it "adds an item to the end of a populated ResizeArray (wrapping indices)" do
      RA.debug2 (RA.snoc (RA.fromArrayAt bottom [ 'a', 'b', 'c' ]) 'd')
        `shouldEqual`
          (top /\ [ bottom /\ 'a', (bottom + 1) /\ 'b', (bottom + 2) /\ 'c', (bottom + 3) /\ 'd' ])

--

  describe "reindex" do
    it "sets the start index of an empty ResizeArray" do
      let
        result = RA.reindex 50 $ RA.fromArray [] :: _ Char
      RA.debug result `shouldEqual`
        { headIndex: Nothing
        , lastIndex: Nothing
        , nextHeadIndex: 50
        , nextLastIndex: 50
        , items: []
        }

    it "sets the start index of a populated ResizeArray" do
      let
        result = RA.reindex 50 $ RA.fromArray [ 'a', 'b', 'c' ]
      RA.debug result `shouldEqual`
        { headIndex: Just 50
        , lastIndex: Just 52
        , nextHeadIndex: 49
        , nextLastIndex: 53
        , items: [ 50 /\ 'a', 51 /\ 'b', 52 /\ 'c' ]
        }

    it "sets the start index to max Int and inidices wrap around" do
      let
        result = RA.reindex top $ RA.fromArray [ 'a', 'b', 'c' ]
      RA.debug result `shouldEqual`
        { headIndex: Just top
        , lastIndex: Just (bottom + 1)
        , nextHeadIndex: top - 1
        , nextLastIndex: bottom + 2
        , items: [ top /\ 'a', bottom /\ 'b', (bottom + 1) /\ 'c' ]
        }

  describe "drop" do
    it "drops more items than the ResizeArray contains" do
      let
        result = RA.drop 5 $ RA.fromArrayAt 10 [ 'a', 'b', 'c' ]
      RA.debug result `shouldEqual`
        { headIndex: Nothing
        , lastIndex: Nothing
        , nextHeadIndex: 13
        , nextLastIndex: 13
        , items: []
        }

    it "drops less items than the ResizeArray contains" do
      let
        result = RA.drop 2 $ RA.fromArray [ 'a', 'b', 'c', 'd', 'e' ]
      RA.debug result `shouldEqual`
        { headIndex: Just 2
        , lastIndex: Just 4
        , nextHeadIndex: 1
        , nextLastIndex: 5
        , items: [ 2 /\ 'c', 3 /\ 'd', 4 /\ 'e' ]
        }

  describe "dropEnd" do
    it "drops more items than the ResizeArray contains" do
      let
        result = RA.dropEnd 5 $ RA.fromArray [ 'a', 'b', 'c' ]
      RA.debug result `shouldEqual`
        { headIndex: Nothing
        , lastIndex: Nothing
        , nextHeadIndex: 0
        , nextLastIndex: 0
        , items: []
        }

    it "drops less items than the ResizeArray contains" do
      let
        result = RA.dropEnd 2 $ RA.fromArray [ 'a', 'b', 'c', 'd', 'e' ]
      RA.debug result `shouldEqual`
        { headIndex: Just 0
        , lastIndex: Just 2
        , nextHeadIndex: -1
        , nextLastIndex: 3
        , items: [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ]
        }

  describe "head" do
    it "returns the head of an empty ResizeArray" do
      let
        result = RA.head $ RA.empty :: _ Char
      result `shouldEqual` Nothing

    it "returns the head of a singleton ResizeArray" do
      let
        result = RA.head $ RA.fromArrayAt 50 [ 'a' ]
      result `shouldEqual` Just 'a'

    it "returns the head of a populated ResizeArray" do
      let
        result = RA.head $ RA.fromArrayAt 50 [ 'a', 'b', 'c' ]
      result `shouldEqual` Just 'a'

  describe "last" do
    it "returns the last item of an empty ResizeArray" do
      let
        result = RA.last $ RA.empty :: _ Char
      result `shouldEqual` Nothing

    it "returns the last item of a singleton ResizeArray" do
      let
        result = RA.last $ RA.fromArrayAt 50 [ 'a' ]
      result `shouldEqual` Just 'a'

    it "returns the last of a populated ResizeArray" do
      let
        result = RA.last $ RA.fromArrayAt 50 [ 'a', 'b', 'c' ]
      result `shouldEqual` Just 'c'

  describe "dropWhile" do
    it "drops all items of ResizeArray of 2 items" do
      let
        result = RA.dropWhile (\_ -> true) $ RA.fromArray [ 'a', 'b' ]
      RA.debug result `shouldEqual`
        { headIndex: Nothing
        , lastIndex: Nothing
        , nextHeadIndex: 2
        , nextLastIndex: 2
        , items: []
        }

    it "drops all items" do
      let
        result = RA.dropWhile (\_ -> true) $ RA.fromArrayAt 10 [ 'a', 'b', 'c' ]
      RA.debug result `shouldEqual`
        { headIndex: Nothing
        , lastIndex: Nothing
        , nextHeadIndex: 13
        , nextLastIndex: 13
        , items: []
        }

    it "drops no items" do
      let
        result = RA.dropWhile (\_ -> false) $ RA.fromArrayAt 10 [ 'a', 'b', 'c' ]
      RA.debug result `shouldEqual`
        { headIndex: Just 10
        , lastIndex: Just 12
        , nextHeadIndex: 9
        , nextLastIndex: 13
        , items: [ 10 /\ 'a', 11 /\ 'b', 12 /\ 'c' ]
        }

  describe "fromArray" do
    it "empty array" do
      let
        result = RA.fromArray [] :: _ Char
      RA.debug result `shouldEqual`
        { headIndex: Nothing
        , lastIndex: Nothing
        , nextHeadIndex: 0
        , nextLastIndex: 0
        , items: []
        }

    it "populated array" do
      let
        result = RA.fromArray [ 'a', 'b', 'c' ]
      RA.debug result `shouldEqual`
        { headIndex: Just 0
        , lastIndex: Just 2
        , nextHeadIndex: -1
        , nextLastIndex: 3
        , items: [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ]
        }

  describe "toList" do
    it "empty ResizeArray" do
      let
        result = RA.toList $ RA.empty :: _ Char
      result `shouldEqual` List.fromFoldable []

    it "populated ResizeArray" do
      let
        result = RA.toList $ RA.fromArray [ 'a', 'b', 'c' ]
      result `shouldEqual` List.fromFoldable [ 'a', 'b', 'c' ]

  describe "cycle" do
    it "" do
      let
        n = 100_000

      let
        items :: ResizeArray Char
        items = RA.fromArray $ Array.replicate n 'a'

      let
        f :: ResizeArray Char -> ResizeArray Char
        f = RA.dropEnd 1 >>> RA.cons 'a'

      let
        go { idx, acc } | idx < 1_000_000 = Loop { idx: idx + 1, acc: f acc }
        go { acc } = Done acc

      let result = tailRec go { idx: 0, acc: items }

      RA.length result `shouldEqual` n
