{-# LANGUAGE NoImplicitPrelude #-}

-- | Type-application-based interface.

module Data.TypeMap.Dynamic.Alt
  ( TypeMap()
  , Item
  , empty
  , null
  , size
  , insert
  , (<:)
  , at
  , update
  , lookup
  , delete
  , union
  , difference
  , intersection
  , map
  , constantList
  , traverse
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
  , constantList
  , OfType
  )

import Data.TypeMap.Internal.Dynamic.Alt
