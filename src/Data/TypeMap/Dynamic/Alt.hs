{-# LANGUAGE NoImplicitPrelude #-}

-- | Type-application-based interface.

module Data.TypeMap.Dynamic.Alt
  ( -- * Dynamic type maps

    TypeMap()
  , Item

    -- ** Basic operations

  , empty
  , null
  , size
  , insert
  , (<:)
  , at
  , update
  , lookup
  , delete

    -- ** Traversals and folds

  , map
  , traverse
  , toList
  , toListMap

    -- ** Set-like operations

  , union
  , difference
  , intersection

    -- * Type-level mappings

  , OfType
  ) where

import Data.TypeMap.Internal.Dynamic
  ( TypeMap
  , Item
  , empty
  , null
  , size
  , union
  , difference
  , intersection
  , toList
  , toListMap
  , OfType
  )

import Data.TypeMap.Internal.Dynamic.Alt
