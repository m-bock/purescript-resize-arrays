module Test.MutArray where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

foreign import data MutArray :: Type -> Type

foreign import fromArrayImpl :: forall a. EffectFn1 (Array a) (MutArray a)

foreign import toArrayImpl :: forall a. EffectFn1 (MutArray a) (Array a)

foreign import shiftImpl :: forall a. EffectFn1 (MutArray a) Unit

foreign import unshiftImpl :: forall a. EffectFn2 a (MutArray a) Unit

foreign import popImpl :: forall a. EffectFn1 (MutArray a) Unit

foreign import pushImpl :: forall a. EffectFn2 a (MutArray a) Unit


foreign import foldlImpl ::  forall a b. EffectFn3 (b -> a -> b) b (MutArray a) b

foreign import lengthImpl :: forall a. MutArray a -> Int

fromArray :: forall a. Array a -> Effect (MutArray a)
fromArray = runEffectFn1 fromArrayImpl

toArray :: forall a. MutArray a -> Effect (Array a)
toArray = runEffectFn1 toArrayImpl

shift :: forall a. MutArray a -> Effect Unit
shift = runEffectFn1 shiftImpl

unshift :: forall a. a -> MutArray a -> Effect Unit
unshift = runEffectFn2 unshiftImpl

pop :: forall a. MutArray a -> Effect Unit
pop = runEffectFn1 popImpl

push :: forall a. a -> MutArray a -> Effect Unit
push = runEffectFn2 pushImpl

foldl :: forall a b. (b -> a -> b) -> b -> MutArray a -> Effect b
foldl = runEffectFn3 foldlImpl

length :: forall a. MutArray a -> Int
length = lengthImpl