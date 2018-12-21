{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.TypeMap.Internal.Map where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.TypeMap.Internal.Unsafe

-- | IntMap-backed type-map.
data TypeMap d = TypeMap !Int !(TypeMap' d) !Int

newtype TypeMap' d = TypeMap' (IntMap Any)

type role TypeMap' nominal

-- | Empty vector.
empty :: TypeMap '[]
empty = TypeMap 0 (TypeMap' IntMap.empty) 0


-- | Access an element indexed by type @a@. /O(log n)/
--
-- If @a@ is associated to @b@ in the type list @d@:
--
-- @
-- 'index' @a (v :: 'TypeMap' d) :: b
-- @
--
-- >>> let v = (0 :: Int) <| True <| "Hello" <| empty :: TypeMap '[ '("a", Int), '("b", Bool), '("c", String)]
-- >>> index @"c" v
-- "Hello"
index
  :: forall a d
  .  KnownNat (Index a d)
  => TypeMap d -> Lookup a d
index (TypeMap inf m1 _) = unsafeIndex @a @d index' m1
  where
    index' :: forall c. IntMap c -> Int -> c
    index' m n = m IntMap.! (inf + n)

-- | Add an element to the beginning of a map. /O(log n)/
cons
  :: forall a d b
  .  b -> TypeMap d -> TypeMap ('(a, b) ': d)
cons b (TypeMap inf m1 sup) =
  TypeMap (inf - 1) (unsafeCons cons' b m1) sup
  where
    cons' :: forall c. c -> IntMap c -> IntMap c
    cons' = IntMap.insert (inf - 1)

-- | Synonym of 'cons'.
(<|)
  :: forall a d b
  .  b -> TypeMap d -> TypeMap ('(a, b) ': d)
(<|) = cons

infixr 5 <|, `cons`

-- | Add an element to the end of a list. /O(log n)/
snoc
  :: forall a d b
  .  (Last d ~ '(a, b))
  => TypeMap (Init d) -> b -> TypeMap d
snoc (TypeMap inf m1 sup) b1 = TypeMap inf (unsafeSnoc @a @d @b snoc' m1 b1) (sup + 1)
  where
    snoc' :: forall c. IntMap c -> c -> IntMap c
    snoc' m b = IntMap.insert sup b m

-- | Synonym of 'snoc'.
(|>)
  :: forall a d b
  .  (Last d ~ '(a, b))
  => TypeMap (Init d) -> b -> TypeMap d
(|>) = snoc

infixl 5 |>, `snoc`
