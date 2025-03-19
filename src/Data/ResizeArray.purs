module Data.ResizeArray
  --   ( ResizeArray
  --   , addEnd
  --   , addStart
  --   , debug
  --   , dropEnd
  --   , dropStart
  --   , dropWhileEnd
  --   , dropWhileStart
  --   , empty
  --   , fromArray
  --   , getEnd
  --   , getStart
  --   , offset
  --   , reindex
  --   ) 

  where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

newtype ResizeArray a = ResizeArray
  { items :: Map Int a
  , anchorIndex :: Int -- The index before the first element
  }

isEmpty :: forall a. ResizeArray a -> Boolean
isEmpty (ResizeArray { items }) = Map.isEmpty items

length :: forall a. ResizeArray a -> Int
length (ResizeArray { items }) = Map.size items

getFirstIndex :: forall a. ResizeArray a -> Maybe Int
getFirstIndex ra@(ResizeArray { anchorIndex }) =
  if isEmpty ra then
    Nothing
  else
    Just (anchorIndex + 1)

getLastIndex :: forall a. ResizeArray a -> Maybe Int
getLastIndex ra@(ResizeArray { anchorIndex }) =
  if isEmpty ra then
    Nothing
  else
    Just (anchorIndex + length ra)

getNextFirstIndex :: forall a. ResizeArray a -> Int
getNextFirstIndex ra@(ResizeArray { anchorIndex }) =
  if isEmpty ra then
    anchorIndex + 1
  else
    anchorIndex

getNextLastIndex :: forall a. ResizeArray a -> Int
getNextLastIndex ra@(ResizeArray { anchorIndex }) =
  if isEmpty ra then
    anchorIndex + 1
  else
    anchorIndex + 1 + length ra

type IndexSnapshot a =
  { firstIndex :: Maybe Int
  , lastIndex :: Maybe Int
  , nextFirstIndex :: Int
  , nextLastIndex :: Int
  , items :: Array (Tuple Int a)
  }

getIndexSnapshot :: forall a. ResizeArray a -> IndexSnapshot a
getIndexSnapshot ra =
  { firstIndex: getFirstIndex ra
  , lastIndex: getLastIndex ra
  , nextFirstIndex: getNextFirstIndex ra
  , nextLastIndex: getNextLastIndex ra
  , items: List.toUnfoldable (toListWithIndices ra)
  }

fromArrayAt :: forall a. Int -> Array a -> ResizeArray a
fromArrayAt firstIndex arr =
  ResizeArray
    { items: foldlWithIndex (\idx acc val -> Map.insert (firstIndex + idx) val acc) Map.empty arr
    , anchorIndex: firstIndex - 1
    }

fromArray :: forall a. Array a -> ResizeArray a
fromArray = fromArrayAt 0

empty :: forall a. ResizeArray a
empty = fromArray []

instance Show a => Show (ResizeArray a) where
  show ra@(ResizeArray { anchorIndex }) =
    let
      arr :: Array a
      arr = List.toUnfoldable (toList ra)

      at = anchorIndex + 1
    in
      "(fromArrayAt " <> show at <> " " <> show arr <> ")"

derive instance Eq a => Eq (ResizeArray a)

derive instance Functor ResizeArray

instance FoldableWithIndex Int ResizeArray where
  foldrWithIndex f z ra = case getLastIndex ra of
    Nothing -> z
    Just lastIndex ->
      mkFoldWithIndex { start: lastIndex, step: \idx -> idx - 1 } f z ra

  foldlWithIndex f z ra = case getFirstIndex ra of
    Nothing -> z
    Just firstIndex ->
      mkFoldWithIndex { start: firstIndex, step: \idx -> idx + 1 } (\idx -> flip $ f idx) z ra

  foldMapWithIndex = foldMapWithIndexDefaultL

mkFoldWithIndex :: forall a b. { start :: Int, step :: Int -> Int } -> (Int -> a -> b -> b) -> b -> ResizeArray a -> b
mkFoldWithIndex { start, step } f z (ResizeArray { items }) = tailRec go { idx: start, acc: z }
  where
  go { idx, acc } = case Map.lookup idx items of
    Just val -> Loop { idx: step idx, acc: f idx val acc }
    Nothing -> Done acc

instance Foldable ResizeArray where
  foldMap = foldMapDefaultL
  foldr f z = foldrWithIndex (\_ val acc -> f val acc) z
  foldl f z = foldlWithIndex (\_ acc val -> f acc val) z

-- getAnchorIndex :: forall a. ResizeArray a -> Int
-- getAnchorIndex (ResizeArray { anchorIndex }) = anchorIndex

toListWithIndices :: forall a. ResizeArray a -> List (Tuple Int a)
toListWithIndices ra = foldrWithIndex (\ix val acc -> List.Cons (Tuple ix val) acc) List.Nil ra

toList :: forall a. ResizeArray a -> List a
toList ra = foldr (\val acc -> List.Cons val acc) List.Nil ra

snoc :: forall a. ResizeArray a -> a -> ResizeArray a
snoc ra@(ResizeArray { items, anchorIndex }) item =
  let
    nextLastIndex = getNextLastIndex ra
  in
    ResizeArray
      { items: Map.insert nextLastIndex item items
      , anchorIndex
      }

cons :: forall a. a -> ResizeArray a -> ResizeArray a
cons item ra@(ResizeArray { items }) =
  let
    nextFirstIndex = getNextFirstIndex ra
  in
    ResizeArray
      { items: Map.insert nextFirstIndex item items
      , anchorIndex: nextFirstIndex - 1
      }

-- reindex :: forall a. Int -> ResizeArray a -> ResizeArray a
-- reindex index ra@(ResizeArray { items }) =
--   toList ra
--     # foldl snoc (fromArrayAt index [])

-- lookup :: forall a. Int -> ResizeArray a -> Maybe a
-- lookup index (ResizeArray { items, anchorIndex }) = do
--   Map.lookup (anchorIndex + index + 1) items

-- -- deleteHead :: forall a. ResizeArray a -> ResizeArray a
-- -- deleteHead ra@(ResizeArray { items, startIndex }) = case Map.lookup startIndex items of
-- --   Nothing -> ra
-- --   Just _ -> ResizeArray
-- --     { items: Map.delete startIndex items
-- --     , startIndex: startIndex + 1
-- --     }

-- -- deleteTail :: forall a. ResizeArray a -> ResizeArray a
-- -- deleteTail ra@(ResizeArray { items, startIndex }) =
-- --   case getEndIndex ra of
-- --     Nothing -> ra
-- --     Just endIndex ->
-- --       ResizeArray
-- --         { items: Map.delete endIndex items
-- --         , startIndex
-- --         }

-- -- getStart :: forall a. ResizeArray a -> Maybe a
-- -- getStart (ResizeArray { items, startIndex }) =
-- --   Map.lookup startIndex items

-- -- getEnd :: forall a. ResizeArray a -> Maybe a
-- -- getEnd ra@(ResizeArray { items }) = do
-- --   endIndex <- getEndIndex ra
-- --   Map.lookup endIndex items

-- -- dropStart :: forall a. Int -> ResizeArray a -> ResizeArray a
-- -- dropStart n ra = tailRec go { n, ra }
-- --   where
-- --   go acc | acc.n == 0 = Done acc.ra
-- --   go acc = Loop { n: acc.n - 1, ra: deleteHead acc.ra }

-- -- dropEnd :: forall a. Int -> ResizeArray a -> ResizeArray a
-- -- dropEnd n ra = tailRec go { n, ra }
-- --   where
-- --   go acc | acc.n == 0 = Done acc.ra
-- --   go acc = Loop { n: acc.n - 1, ra: deleteTail acc.ra }

-- -- dropWhileStart :: forall a. (a -> Boolean) -> ResizeArray a -> ResizeArray a
-- -- dropWhileStart f ra = tailRec go ra
-- --   where
-- --   go acc = case getStart acc of
-- --     Just item | f item -> Loop (deleteHead acc)
-- --     _ -> Done acc

-- -- dropWhileEnd :: forall a. (a -> Boolean) -> ResizeArray a -> ResizeArray a
-- -- dropWhileEnd f ra = tailRec go ra
-- --   where
-- --   go acc = case getEnd acc of
-- --     Just item | f item -> Loop (deleteTail acc)
-- --     _ -> Done acc

-- -- ----

-- getNextTailIndex :: forall a. ResizeArray a -> Int
-- getNextTailIndex ra@(ResizeArray { anchorIndex }) = anchorIndex + length ra

-- getNextFirstIndex :: forall a. ResizeArray a -> Int
-- getNextFirstIndex ra@(ResizeArray { anchorIndex }) = anchorIndex

-- getFirstIndex :: forall a. ResizeArray a -> Maybe Int
-- getFirstIndex ra | isEmpty ra = Nothing
-- getFirstIndex ra@(ResizeArray { anchorIndex }) = Just (anchorIndex + 1)

-- -- getNextStartIndex :: forall a. ResizeArray a -> Int
-- -- getNextStartIndex ra@(ResizeArray { startIndex }) =
-- --   if isEmpty ra then
-- --     startIndex
-- --   else
-- --     startIndex - 1

-- getLastIndex :: forall a. ResizeArray a -> Maybe Int
-- getLastIndex ra | isEmpty ra = Nothing
-- getLastIndex ra@(ResizeArray { anchorIndex }) = Just (anchorIndex + length ra)
