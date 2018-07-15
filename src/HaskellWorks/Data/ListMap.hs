{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaskellWorks.Data.ListMap where

import Control.Lens
import Data.Monoid
import Prelude      hiding (lookup, null)

import qualified Prelude as P

newtype ListMap a = ListMap [(String, a)] deriving (Eq, Show)

instance Functor ListMap where
  fmap f = mapWithKey (\_ v -> f v)
  {-# INLINE fmap #-}

instance Foldable ListMap where
  foldMap f (ListMap as) = foldMap f (snd <$> as)
  {-# INLINE foldMap #-}
  foldr f z (ListMap as) = foldr f z (snd <$> as)
  {-# INLINE foldr #-}


instance Traversable ListMap where
  traverse f = traverseWithKey (\_ v -> f v)
  {-# INLINE traverse #-}


type instance Index (ListMap a) = String
type instance IxValue (ListMap a) = a

instance Ixed (ListMap a) where
  ix k f m = case lookup k m of
     Just v  -> f v <&> \v' -> insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance At (ListMap a) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m
    where mv = lookup k m
  {-# INLINE at #-}

-- | @'each' :: 'Traversal' ('Map' c a) ('Map' c b) a b@
instance Each (ListMap a) (ListMap b) a b where
  each = traversed
  {-# INLINE each #-}

instance AsEmpty (ListMap a) where
  _Empty = nearly empty null
  {-# INLINE _Empty #-}

instance FunctorWithIndex String ListMap
instance FoldableWithIndex String ListMap
instance TraversableWithIndex String ListMap where
#if MIN_VERSION_containers(0,5,0)
  itraverse = traverseWithKey
#else
  itraverse f = sequenceA . IntMap.mapWithKey f
#endif
  {-# INLINE [0] itraverse #-}

mapWithKey :: (String -> a -> b) -> ListMap a -> ListMap b
mapWithKey f (ListMap as) = ListMap $ (\(k, v) -> (k, f k v)) <$> as
{-# INLINE mapWithKey #-}

traverseWithKey :: Applicative t => (String -> a -> t b) -> ListMap a -> t (ListMap b)
traverseWithKey f (ListMap as) = ListMap <$> traverse (\(k, v) -> (k,) <$> f k v) as
{-# INLINE traverseWithKey #-}

empty :: ListMap a
empty = ListMap []
{-# INLINE empty #-}

null :: ListMap a -> Bool
null (ListMap xs) = P.null xs
{-# INLINE null #-}

fromList :: [(String, a)] -> ListMap a
fromList = ListMap
{-# INLINE fromList #-}

toList :: ListMap a -> [(String, a)]
toList (ListMap m) = m
{-# INLINE toList #-}

insert :: String -> a -> ListMap a -> ListMap a
insert k v (ListMap m) = ListMap $ case break (\(k', _) -> k' == k) m of
  (ps, _:xs) -> ps <> ((k, v):xs)
  _          -> (k, v) : m
{-# INLINE insert #-}

delete :: String -> ListMap a -> ListMap a
delete k (ListMap m) = ListMap $ case break (\(k', _) -> k' == k) m of
  (ps, _:xs) -> ps <> xs
  _          -> m
{-# INLINE delete #-}

lookup :: String -> ListMap a -> Maybe a
lookup k (ListMap m) = P.lookup k m
{-# INLINE lookup #-}
