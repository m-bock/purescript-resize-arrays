module Test.Specs where

import Prelude

import Data.Foldable (foldl, foldr)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.ResizeArray as RA
import Data.Tuple.Nested ((/\))
import Debug (spyWith)
import Test.Spec (Spec, describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

maxInt :: Int
maxInt = 2147483647

minInt :: Int
minInt = -2147483648

spec :: Spec Unit
spec = do
  describe "show" do
    it "empty ResizeArray" do
      let
        items = RA.empty :: _ Char
      show items `shouldEqual` "(fromArrayAt 0 [])"

    it "populated ResizeArray" do
      let
        items = RA.fromArray [ 'a', 'b', 'c' ]
      show items `shouldEqual` "(fromArrayAt 0 ['a','b','c'])"

  describe "empty" do
    it "has a length of zero" do
      let
        result = RA.empty :: _ Char
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Nothing
        , lastIndex: Nothing
        , nextFirstIndex: 0
        , nextLastIndex: 0
        , items: []
        }

  describe "fromArrayAtAnchor" do
    it "start index zero, empty array" do
      let
        result = RA.fromArrayAt 0 [] :: _ Char
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Nothing
        , lastIndex: Nothing
        , nextFirstIndex: 0
        , nextLastIndex: 0
        , items: []
        }

    it "start index zero, populated array" do
      let
        result = RA.fromArrayAt 0 [ 'a', 'b', 'c' ]
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just 0
        , lastIndex: Just 2
        , nextFirstIndex: -1
        , nextLastIndex: 3
        , items: [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ]
        }

    it "start index non zero, empty array" do
      let
        result = RA.fromArrayAt 10 [] :: _ Char
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Nothing
        , lastIndex: Nothing
        , nextFirstIndex: 10
        , nextLastIndex: 10
        , items: []
        }

    it "start index non zero, populated array" do
      let
        result = RA.fromArrayAt 10 [ 'a', 'b', 'c' ]
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just 10
        , lastIndex: Just 12
        , nextFirstIndex: 9
        , nextLastIndex: 13
        , items: [ 10 /\ 'a', 11 /\ 'b', 12 /\ 'c' ]
        }

  describe "eq" do
    it "empty ResizeArray" do
      let
        items1 = RA.empty :: _ Char
        items2 = RA.empty :: _ Char

      items1 `shouldEqual` items2

    it "ResizeArrays with equal start index and equal items are considered equal" do
      let
        items1 = RA.fromArrayAt 0 [ 'a', 'b', 'c' ]
        items2 = RA.fromArrayAt 0 [ 'a', 'b', 'c' ]

      items1 `shouldEqual` items2

    it "ResizeArrays different start index are considered inequal" do
      let
        items1 = RA.fromArrayAt 0 [ 'a', 'b', 'c' ]
        items2 = RA.fromArrayAt 10 [ 'a', 'b', 'c' ]

      items1 `shouldNotEqual` items2

    it "ResizeArrays with different items are considered inequal" do
      let
        items1 = RA.fromArrayAt 0 [ 'a', 'b', 'c' ]
        items2 = RA.fromArrayAt 0 [ 'a', 'b', 'd' ]

      items1 `shouldNotEqual` items2

  describe "map" do
    it "maps over empty ResizeArray" do
      let
        mapFn val = val
        items = RA.empty :: _ Char
        result = map mapFn items

      result `shouldEqual` RA.empty

    it "maps over populated ResizeArray" do
      let
        mapFn val = val <> "!"
        items = RA.fromArray [ "a", "b", "c" ]
        result = map mapFn items

      result `shouldEqual` RA.fromArray [ "a!", "b!", "c!" ]

  describe "foldlWithIndex" do
    it "accumulates with index from left to right" do
      let
        accumFn idx acc val = acc <> [ idx /\ val ]
        init = []
        items = RA.fromArray [ 'a', 'b', 'c' ]

        result = foldlWithIndex accumFn init items

      result `shouldEqual` [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ]

  describe "foldrWithIndex" do
    it "accumulates with index from right to left" do
      let
        accumFn idx val acc = acc <> [ idx /\ val ]
        init = []
        items = RA.fromArray [ 'a', 'b', 'c' ]

        result = foldrWithIndex accumFn init items

      result `shouldEqual` [ 2 /\ 'c', 1 /\ 'b', 0 /\ 'a' ]

  describe "foldl" do
    it "accumulates from left to right" do
      let
        accumFn acc val = acc <> [ val ]
        init = []
        items = RA.fromArray [ 'a', 'b', 'c' ]

        result = foldl accumFn init items

      result `shouldEqual` [ 'a', 'b', 'c' ]

  describe "foldr" do
    it "accumulates from right to left" do
      let
        accumFn val acc = acc <> [ val ]
        init = []
        items = RA.fromArray [ 'a', 'b', 'c' ]

        result = foldr accumFn init items

      result `shouldEqual` [ 'c', 'b', 'a' ]

  describe "cons" do
    it "adds an item to the start of an empty ResizeArray" do
      let
        result = RA.cons 'a' $ RA.empty :: _ Char
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just 0
        , lastIndex: Just 0
        , nextFirstIndex: -1
        , nextLastIndex: 1
        , items: [ 0 /\ 'a' ]
        }
    it "adds an item to the start of a populated ResizeArray" do
      let
        result = RA.cons 'a' $ RA.fromArray [ 'b', 'c', 'd' ]
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just (-1)
        , lastIndex: Just 2
        , nextFirstIndex: -2
        , nextLastIndex: 3
        , items: [ -1 /\ 'a', 0 /\ 'b', 1 /\ 'c', 2 /\ 'd' ]
        }

    it "adds an item to the start of a populated ResizeArray (wrapping indices)" do
      let
        result = RA.cons 'a' $ RA.fromArrayAt bottom [ 'b', 'c', 'd' ]
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just top
        , lastIndex: Just (bottom + 2)
        , nextFirstIndex: top - 1
        , nextLastIndex: bottom + 3
        , items: [ top /\ 'a', bottom /\ 'b', (bottom + 1) /\ 'c', (bottom + 2) /\ 'd' ]
        }

  describe "snoc" do
    it "adds an item to the end of an empty ResizeArray" do
      let
        result = RA.snoc (RA.empty :: _ Char) 'a'
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just 0
        , lastIndex: Just 0
        , nextFirstIndex: -1
        , nextLastIndex: 1
        , items: [ 0 /\ 'a' ]
        }

    it "adds an item to the end of a populated ResizeArray" do
      let
        result = RA.snoc (RA.fromArray [ 'a', 'b', 'c' ]) 'd'
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just 0
        , lastIndex: Just 3
        , nextFirstIndex: -1
        , nextLastIndex: 4
        , items: [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c', 3 /\ 'd' ]
        }

    it "adds an item to the end of a populated ResizeArray (wrapping indices)" do
      let
        result = RA.snoc (RA.fromArrayAt (top - 2) [ 'a', 'b', 'c' ]) 'd'
      RA.getIndexSnapshot result `shouldEqual`
        { firstIndex: Just (top - 2)
        , lastIndex: Just bottom
        , nextFirstIndex: top - 3
        , nextLastIndex: bottom + 1
        , items: [ (top - 2) /\ 'a', (top - 1) /\ 'b', top /\ 'c', bottom /\ 'd' ]
        }

-- describe "fromArray" do
--   it "empty array" do
--     let
--       result = RA.fromArray [] :: _ Char
--     RA.indexSnapshot result `shouldEqual` { startIndex: 0, items: [] }
--   it "populated array" do
--     let
--       result = RA.fromArray [ 'a', 'b', 'c' ]
--     RA.indexSnapshot result `shouldEqual` { startIndex: 0, items: [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ] }

-- describe "toList" do
--   it "empty ResizeArray" do
--     let
--       result = RA.toList $ RA.empty :: _ Char
--     result `shouldEqual` List.fromFoldable []

--   it "populated ResizeArray" do
--     let
--       result = RA.toList $ RA.fromArray [ 'a', 'b', 'c' ]
--     result `shouldEqual` List.fromFoldable [ 'a', 'b', 'c' ]

-- describe "setStartIndex" do
--   it "sets the start index of an empty ResizeArray" do
--     let
--       result = RA.setStartIndex 50 $ RA.fromArray [] :: _ Char
--     RA.indexSnapshot result `shouldEqual` { startIndex: 50, items: [] }

--   it "sets the start index of a populated ResizeArray" do
--     let
--       result = RA.setStartIndex 50 $ RA.fromArray [ 'a', 'b', 'c' ]
--     RA.indexSnapshot result `shouldEqual` { startIndex: 50, items: [ 50 /\ 'a', 51 /\ 'b', 52 /\ 'c' ] }

--   it "sets the start index to maxInt and inidices wrap around" do
--     let
--       result = RA.setStartIndex maxInt $ RA.fromArray [ 'a', 'b', 'c' ]
--     RA.indexSnapshot result `shouldEqual` { startIndex: 2147483647, items: [ 2147483647 /\ 'a', -2147483648 /\ 'b', -2147483647 /\ 'c' ] }

-- describe "lookup" do
--   it "does not find non existing index" do
--     let
--       result = RA.lookup 50 $ RA.fromArray [ 'a', 'b', 'c' ]
--     result `shouldEqual` Nothing

--   it "finds existing index" do
--     let
--       result = RA.lookup 1 $ RA.fromArray [ 'a', 'b', 'c' ]
--     result `shouldEqual` Just 'b'

-- describe "cons" do
--   it "adds an item to the start of an empty ResizeArray" do
--     let
--       result = RA.cons 'a' $ RA.empty :: _ Char
--     RA.indexSnapshot result `shouldEqual` { startIndex: 0, items: [ 0 /\ 'a' ] }

--   it "adds an item to the start of a populated ResizeArray" do
--     let
--       result = RA.cons 'a' $ RA.fromArray [ 'b', 'c', 'd' ]
--     RA.indexSnapshot result `shouldEqual` { startIndex: -1, items: [ -1 /\ 'a', 0 /\ 'b', 1 /\ 'c', 2 /\ 'd' ] }

-- describe "snoc" do
--   it "adds an item to the end of an empty ResizeArray" do
--     let
--       result = RA.snoc (RA.empty :: _ Char) 'a'
--     RA.indexSnapshot result `shouldEqual` { startIndex: 0, items: [ 0 /\ 'a' ] }

--   it "adds an item to the end of a populated ResizeArray" do
--     let
--       result = RA.snoc (RA.fromArray [ 'a', 'b', 'c' ]) 'd'
--     RA.indexSnapshot result `shouldEqual` { startIndex: 0, items: [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c', 3 /\ 'd' ] }