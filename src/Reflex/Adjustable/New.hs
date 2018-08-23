{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Adjustable.New where

import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Reflex
import Reflex.Patch.DMapWithMove (NodeInfo)
import Unsafe.Coerce

newtype ConstUnit (a :: *) (u :: ()) = ConstUnit
  { unConstUnit :: a }
  deriving (Eq, Ord, Show)

newtype IntMapT (v :: () -> *) = IntMapT
  { unIntMapT :: IntMap (v '()) }

class Ord (SomeKey c) => Container (c :: (k -> *) -> *) where
  type Key c :: k -> *
  type SomeKey c :: *
  someKey :: Key c a -> SomeKey c
  mapWithKey :: (forall a. Key c a -> f a -> f' a) -> c f -> c f'
  mapMaybeWithKey :: (forall a. Key c a -> f a -> Maybe (f' a)) -> c f -> c f'
  traverseWithKey :: Applicative m => (forall a. Key c a -> f a -> m (f' a)) -> c f -> m (c f')
  lookup :: Key c a -> c f -> Maybe (f a)
  null :: c f -> Bool
  insert :: Key c a -> f a -> c f -> c f
  delete :: Key c a -> c f -> c f
  deleteSome :: SomeKey c -> c f -> c f
  member :: Key c a -> c f -> Bool
  memberSome :: SomeKey c -> c f -> Bool
  toList :: c f -> [DSum (Key c) f]
  toAscList :: c f -> [DSum (Key c) f]
  toDescList :: c f -> [DSum (Key c) f]
  fromList :: [DSum (Key c) f] -> c f
  fromAscList :: [DSum (Key c) f] -> c f

instance Container IntMapT where
  type Key IntMapT = ConstUnit IntMap.Key
  type SomeKey IntMapT = IntMap.Key
  someKey = unConstUnit
  mapWithKey f = IntMapT . IntMap.mapWithKey (f . ConstUnit) . unIntMapT
  mapMaybeWithKey f = IntMapT . IntMap.mapMaybeWithKey (f . ConstUnit) . unIntMapT
  traverseWithKey f = fmap IntMapT . IntMap.traverseWithKey (f . ConstUnit) . unIntMapT
  lookup (ConstUnit k) = fmap coerceUnit . IntMap.lookup k . unIntMapT
  null = IntMap.null . unIntMapT
  insert (ConstUnit k) v = IntMapT . IntMap.insert k (coerceUnit v) . unIntMapT
  delete (ConstUnit k) = IntMapT . IntMap.delete k . unIntMapT
  deleteSome k = IntMapT . IntMap.delete k . unIntMapT
  member (ConstUnit k) = IntMap.member k . unIntMapT
  memberSome k = IntMap.member k . unIntMapT
  toList = map (\(k, v) -> ConstUnit k :=> coerceUnit v) . IntMap.toList . unIntMapT
  toAscList = map (\(k, v) -> ConstUnit k :=> coerceUnit v) . IntMap.toAscList . unIntMapT
  toDescList = map (\(k, v) -> ConstUnit k :=> coerceUnit v) . IntMap.toDescList . unIntMapT
  fromList = IntMapT . IntMap.fromList . map (\(ConstUnit k :=> v) -> (k, coerceUnit v))
  fromAscList = IntMapT . IntMap.fromAscList . map (\(ConstUnit k :=> v) -> (k, coerceUnit v))

instance DMap.GCompare k => Container (DMap k) where
  type Key (DMap k) = k
  type SomeKey (DMap k) = DMap.Some k
  someKey = DMap.This
  mapWithKey = DMap.mapWithKey
  mapMaybeWithKey = DMap.mapMaybeWithKey
  traverseWithKey = DMap.traverseWithKey
  lookup = DMap.lookup
  null = DMap.null
  insert = DMap.insert
  delete = DMap.delete
  deleteSome (DMap.This k) = DMap.delete k
  member = DMap.member
  memberSome (DMap.This k) = DMap.member k
  toList = DMap.toList
  toAscList = DMap.toAscList
  toDescList = DMap.toDescList
  fromList = DMap.fromList
  fromAscList = DMap.fromAscList

coerceUnit :: forall a b (f :: () -> *). f a -> f b
coerceUnit = unsafeCoerce

class (Reflex t, Monad m) => ContainerAdjustable t m where
  traverseContainerWithKeyWithAdjustWithMove
    :: Container c
    => (forall a. Key c a -> v a -> m (v' a))
    -> c v
    -> Event t (c (NodeInfo (Key c) v))
    -> m (c v', Event t (c (NodeInfo (Key c) v')))
