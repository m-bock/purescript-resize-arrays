module Data.N where

import Prelude

import Data.Int as Int
import Dodo.Box (valign)

maxLength :: Int
maxLength = (Int.pow 32 2) / 2

min :: Int
min = -(maxLength / 2)

max :: Int
max = (maxLength / 2) - 1

newtype WrappingInt = WrappingInt Int

zero :: WrappingInt
zero = WrappingInt 0

add :: WrappingInt -> WrappingInt -> WrappingInt
add (WrappingInt i) (WrappingInt j) =
  let
    r = i + j
  in
    if r > max then
      WrappingInt $ r - maxLength
    else if r < min then
      WrappingInt $ r + maxLength
    else
      WrappingInt r

inc :: WrappingInt -> WrappingInt
inc (WrappingInt i) = add (WrappingInt i) (WrappingInt 1)

dec :: WrappingInt -> WrappingInt
dec (WrappingInt i) = add (WrappingInt i) (WrappingInt (-1))

sub :: WrappingInt -> WrappingInt -> WrappingInt
sub (WrappingInt i) (WrappingInt j) = add (WrappingInt i) (WrappingInt (-j))