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
import Data.Array.NonEmpty (toArray)
import Data.Foldable (class Foldable, foldMapDefaultL, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

newtype ResizeArray a = ResizeArray
  { items :: Map Int a
  , startIndex :: Int
  }

derive instance Functor ResizeArray

instance Show a => Show (ResizeArray a) where
  show ra@(ResizeArray { startIndex }) = "(reindex " <> show startIndex <> "(fromArray " <> show (List.toUnfoldable (toList ra) :: Array _) <> "))"

instance FoldableWithIndex Int ResizeArray where
  foldrWithIndex f z ra@(ResizeArray { items }) = case getEndIndex ra of
    Nothing -> z
    Just endIndex -> tailRec go { idx: endIndex, acc: z }
    where
    go { idx, acc } = case Map.lookup idx items of
      Just val -> Loop { idx: idx - 1, acc: f idx val acc }
      Nothing -> Done acc

  foldlWithIndex f z (ResizeArray { items, startIndex }) = tailRec go { idx: startIndex, acc: z }
    where
    go { idx, acc } = case Map.lookup idx items of
      Just val -> Loop { idx: idx + 1, acc: f idx acc val }
      Nothing -> Done acc

  foldMapWithIndex = foldMapWithIndexDefaultL

instance Foldable ResizeArray where
  foldMap = foldMapDefaultL
  foldr f z = foldrWithIndex (\_ val acc -> f val acc) z
  foldl f z = foldlWithIndex (\_ acc val -> f acc val) z

empty :: forall a. ResizeArray a
empty = ResizeArray
  { items: Map.empty
  , startIndex: 0
  }

fromArray :: forall a. Array a -> ResizeArray a
fromArray arr =
  ResizeArray
    { items: foldlWithIndex (\idx acc val -> Map.insert idx val acc) Map.empty arr
    , startIndex: 0
    }

toListWithIndices :: forall a. ResizeArray a -> List (Tuple Int a)
toListWithIndices ra = foldrWithIndex (\ix val acc -> List.Cons (Tuple ix val) acc) List.Nil ra

toList :: forall a. ResizeArray a -> List a
toList ra = foldr (\val acc -> List.Cons val acc) List.Nil ra

reindex :: forall a. Int -> ResizeArray a -> ResizeArray a
reindex newStartIndex (ResizeArray { items, startIndex }) =
  ResizeArray
    { items: tailRec go { idx: 0, acc: Map.empty }
    , startIndex: newStartIndex
    }
  where
  go { idx, acc } = case Map.lookup (startIndex + idx) items of
    Just val -> Loop { idx: idx + 1, acc: Map.insert (newStartIndex + idx) val acc }
    Nothing -> Done acc

lookup :: forall a. Int -> ResizeArray a -> Maybe a
lookup index (ResizeArray { items, startIndex }) = do
  Map.lookup (startIndex + index) items

addStart :: forall a. a -> ResizeArray a -> ResizeArray a
addStart item ra@(ResizeArray { items }) =
  let
    nextStartIndex = getNextStartIndex ra
  in
    ResizeArray
      { items: Map.insert nextStartIndex item items
      , startIndex: nextStartIndex
      }

addEnd :: forall a. a -> ResizeArray a -> ResizeArray a
addEnd item ra@(ResizeArray { items, startIndex }) =
  let
    nextEndIndex = getNextEndIndex ra
  in
    ResizeArray
      { items: Map.insert nextEndIndex item items
      , startIndex
      }

isEmpty :: forall a. ResizeArray a -> Boolean
isEmpty (ResizeArray { items }) = Map.isEmpty items

deleteStart :: forall a. ResizeArray a -> ResizeArray a
deleteStart ra@(ResizeArray { items, startIndex }) = unsafeCoerce 1

-- case getStartIndex ra of
--   Nothing -> ra
--   Just startIndex ->
--     ResizeArray
--       { items: Map.delete startIndex items
--       , startIndex: startIndex + 1
--       }

deleteEnd :: forall a. ResizeArray a -> ResizeArray a
deleteEnd ra@(ResizeArray { items, startIndex }) =
  case getEndIndex ra of
    Nothing -> ra
    Just endIndex ->
      ResizeArray
        { items: Map.delete endIndex items
        , startIndex
        }

getStart :: forall a. ResizeArray a -> Maybe a
getStart (ResizeArray { items, startIndex }) =
  Map.lookup startIndex items

getEnd :: forall a. ResizeArray a -> Maybe a
getEnd ra@(ResizeArray { items }) = do
  endIndex <- getEndIndex ra
  Map.lookup endIndex items

dropStart :: forall a. Int -> ResizeArray a -> ResizeArray a
dropStart n ra = tailRec go { n, ra }
  where
  go acc | acc.n == 0 = Done acc.ra
  go acc = Loop { n: acc.n - 1, ra: deleteStart acc.ra }

dropEnd :: forall a. Int -> ResizeArray a -> ResizeArray a
dropEnd n ra = tailRec go { n, ra }
  where
  go acc | acc.n == 0 = Done acc.ra
  go acc = Loop { n: acc.n - 1, ra: deleteEnd acc.ra }

dropWhileStart :: forall a. (a -> Boolean) -> ResizeArray a -> ResizeArray a
dropWhileStart f ra = tailRec go ra
  where
  go acc = case getStart acc of
    Just item | f item -> Loop (deleteStart acc)
    _ -> Done acc

dropWhileEnd :: forall a. (a -> Boolean) -> ResizeArray a -> ResizeArray a
dropWhileEnd f ra = tailRec go ra
  where
  go acc = case getEnd acc of
    Just item | f item -> Loop (deleteEnd acc)
    _ -> Done acc

length :: forall a. ResizeArray a -> Int
length (ResizeArray { items }) = Map.size items

----

getNextEndIndex :: forall a. ResizeArray a -> Int
getNextEndIndex ra@(ResizeArray { startIndex }) = startIndex + length ra + 1

getStartIndex :: forall a. ResizeArray a -> Int
getStartIndex ra@(ResizeArray { startIndex }) = startIndex

getNextStartIndex :: forall a. ResizeArray a -> Int
getNextStartIndex ra@(ResizeArray { startIndex }) = startIndex - 1

getEndIndex :: forall a. ResizeArray a -> Maybe Int
getEndIndex ra | isEmpty ra = Nothing
getEndIndex ra@(ResizeArray { startIndex }) = Just (startIndex + length ra - 1)
