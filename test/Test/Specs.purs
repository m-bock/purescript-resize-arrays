module Test.Specs where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.ResizeArray as RA
import Data.Tuple.Nested ((/\))
import Debug (spyWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

maxInt :: Int
maxInt = 2147483647

minInt :: Int
minInt = -2147483648

spec :: Spec Unit
spec = do
  describe "foldrWithIndex" do
    it ".." do
      let
        result = foldrWithIndex (\idx val acc -> acc <> [idx /\ val]) [] $ spyWith "" show  $ RA.fromArray [ 'a', 'b', 'c' ]
      result `shouldEqual`  [2 /\ 'c', 1 /\ 'b', 0 /\ 'a']

  describe "foldlWithIndex" do
    it ".." do
      let
        result = foldlWithIndex (\idx acc val -> acc <> [idx /\ val]) [] $ spyWith "" show  $ RA.fromArray [ 'a', 'b', 'c' ]
      result `shouldEqual`  [0 /\ 'a', 1 /\ 'b', 2 /\ 'c']      

  describe "empty" do
    it ".." do
      let
        result = RA.empty :: _ Char
      RA.toListWithIndices result `shouldEqual` List.fromFoldable []
      RA.getStartIndex result `shouldEqual` 0

  describe "fromArray" do
    it ".." do
      let
        result = RA.fromArray [] :: _ Char
      RA.toListWithIndices result `shouldEqual` List.fromFoldable []
      RA.getStartIndex result `shouldEqual` 0

    it ".." do
      let
        result = RA.fromArray [ 'a', 'b', 'c' ]
      RA.toListWithIndices result `shouldEqual` List.fromFoldable [ 0 /\ 'a', 1 /\ 'b', 2 /\ 'c' ]
      RA.getStartIndex result `shouldEqual` 0

  describe "toArray" do
    it ".." do
      let
        result = RA.fromArray [] :: _ Char
      RA.toListWithIndices result `shouldEqual` List.fromFoldable []
      RA.getStartIndex result `shouldEqual` 0

    it ".." do
      let
        result = RA.fromArray [ 'a', 'b', 'c' ]
      RA.toList result `shouldEqual` List.fromFoldable [ 'a', 'b', 'c' ]
      RA.getStartIndex result `shouldEqual` 0

  describe "reindex" do
    it ".." do
      let
        result = RA.reindex 50 $ RA.fromArray [] :: _ Char
      RA.toList result `shouldEqual` List.fromFoldable []
      RA.getStartIndex result `shouldEqual` 50

    it ".." do
      let
        result = RA.reindex 50 $ RA.fromArray [ 'a', 'b', 'c' ]
      RA.toListWithIndices result `shouldEqual` List.fromFoldable [ 50 /\ 'a', 51 /\ 'b', 52 /\ 'c' ]
      RA.getStartIndex result `shouldEqual` 50

    it ".." do
      let
        result = spyWith "ss" show $  RA.reindex maxInt $ RA.fromArray [ 'a', 'b', 'c' ]
      RA.toListWithIndices result `shouldEqual` List.fromFoldable [ maxInt /\ 'a', minInt /\ 'b', (minInt + 1) /\ 'c' ]
      RA.getStartIndex result `shouldEqual` maxInt

  describe "lookup" do
    it ".." do
      let
        result = RA.lookup 50 $ RA.fromArray [] :: _ Char
      result `shouldEqual` Nothing

    it ".." do
      let
        result = RA.lookup 1 $ RA.fromArray [ 'a', 'b', 'c' ]
      result `shouldEqual` Just 'b'

--     RA.debug (RA.empty :: _ Char) `shouldEqual`
--       { startIndex: 0
--       , items: []
--       }

-- describe "reindex" do
--   it ".." do
--     RA.debug (RA.reindex 50 $ RA.empty :: _ Char) `shouldEqual`
--       { startIndex: 50
--       , items: []
--       }
