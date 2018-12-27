{-# LANGUAGE NoImplicitPrelude #-}

module Data.TypeMap.Dynamic
  ( TypeMap()
  , Item
  , empty
  , null
  , size
  , insert
  , (<:)
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
