module Data.Sprite where

import Prelude hiding (div)
import Data.Array as Array
import Data.Identity (Identity(..)) -- TODO eliminate

type Sprite p = Array (Array p)

modifyAt :: forall a. Array (Array a) -> (a -> a) -> Int -> Int -> Array (Array a)
modifyAt spr f x y =
  Array.modifyAtIndices (at y) (Array.modifyAtIndices (at x) f) spr
  where at = Identity -- TODO eliminate
