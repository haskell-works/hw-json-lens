{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Alexey Raga 2016, (c) Edward Kmett 2013-2014, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Alexey Raga <alexey.raga@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------

module HaskellWorks.Data.Json.Lens where

import Control.Applicative
import Control.Arrow                       (first)
import Control.Lens
import Data.Data
import Data.Scientific                     (Scientific)
import GHC.Base
import HaskellWorks.Data.Json.PartialValue as J
import HaskellWorks.Data.ListMap           (ListMap, fromList, toList)
import Prelude                             hiding (null)

import qualified Data.Scientific as Scientific
import qualified Data.Text       as T

------------------------------------------------------------------------------
-- Scientific prisms
------------------------------------------------------------------------------

class AsNumber t where
  _Number :: Prism' t Scientific
#ifndef HLINT
  default _Number :: AsPrimitive t => Prism' t Scientific
  _Number = _Primitive._Number
  {-# INLINE _Number #-}
#endif

  -- |
  -- Prism into an 'Double' over a 'Value', 'Primitive' or 'Scientific'
  _Double :: Prism' t Double
  _Double = _Number.iso Scientific.toRealFloat realToFrac
  {-# INLINE _Double #-}

  -- |
  -- Prism into an 'Integer' over a 'Value', 'Primitive' or 'Scientific'
  _Integer :: Prism' t Integer
  _Integer = _Number.iso floor fromIntegral
  {-# INLINE _Integer #-}

instance AsNumber JsonPartialValue where
  _Number = prism (JsonPartialNumber . realToFrac) $ \v -> case v of
    JsonPartialNumber n -> Right (Scientific.fromFloatDigits n)
    _                   -> Left v
  {-# INLINE _Number #-}

instance AsNumber Scientific where
  _Number = id
  {-# INLINE _Number #-}

------------------------------------------------------------------------------
-- Conversion Prisms
------------------------------------------------------------------------------

-- | Access Integer 'Value's as Integrals.
_Integral :: (AsNumber t, Integral a) => Prism' t a
_Integral = _Number . iso floor fromIntegral
{-# INLINE _Integral #-}

------------------------------------------------------------------------------
-- Null values and primitives
------------------------------------------------------------------------------

-- | Primitives of 'Value'
data Primitive
  = StringPrim !String
  | NumberPrim !Scientific
  | BoolPrim !Bool
  | NullPrim
  deriving (Eq,Ord,Show,Data,Typeable)

instance AsNumber Primitive where
  _Number = prism NumberPrim $ \v -> case v of NumberPrim s -> Right s; _ -> Left v
  {-# INLINE _Number #-}

class AsNumber t => AsPrimitive t where
  _Primitive :: Prism' t Primitive
#ifndef HLINT
  default _Primitive :: AsValue t => Prism' t Primitive
  _Primitive = _Value._Primitive
  {-# INLINE _Primitive #-}
#endif

  _String :: Prism' t String
  _String = _Primitive.prism StringPrim (\v -> case v of StringPrim s -> Right s; _ -> Left v)
  {-# INLINE _String #-}

  _Bool :: Prism' t Bool
  _Bool = _Primitive.prism BoolPrim (\v -> case v of BoolPrim b -> Right b; _ -> Left v)
  {-# INLINE _Bool #-}

  _Null :: Prism' t ()
  _Null = _Primitive.prism (const NullPrim) (\v -> case v of NullPrim -> Right (); _ -> Left v)
  {-# INLINE _Null #-}


instance AsPrimitive JsonPartialValue where
  _Primitive = prism fromPrim toPrim
    where toPrim (JsonPartialString s) = Right $ StringPrim (T.unpack s)
          toPrim (JsonPartialNumber n) = Right $ NumberPrim (Scientific.fromFloatDigits n)
          toPrim (JsonPartialBool b)   = Right $ BoolPrim b
          toPrim JsonPartialNull       = Right NullPrim
          toPrim v                     = Left v
          {-# INLINE toPrim #-}
          fromPrim (StringPrim s) = JsonPartialString (T.pack s)
          fromPrim (NumberPrim n) = JsonPartialNumber (realToFrac n)
          fromPrim (BoolPrim b)   = JsonPartialBool b
          fromPrim NullPrim       = JsonPartialNull
          {-# INLINE fromPrim #-}
  {-# INLINE _Primitive #-}
  _String = prism (JsonPartialString . T.pack) $ \v -> case v of JsonPartialString s -> Right (T.unpack s); _ -> Left v
  {-# INLINE _String #-}
  _Bool = prism JsonPartialBool (\v -> case v of JsonPartialBool b -> Right b; _ -> Left v)
  {-# INLINE _Bool #-}
  _Null = prism (const JsonPartialNull) (\v -> case v of JsonPartialNull -> Right (); _ -> Left v)
  {-# INLINE _Null #-}

instance AsPrimitive Primitive where
  _Primitive = id
  {-# INLINE _Primitive #-}

-- | Prism into non-'Null' values
nonNull :: Prism' JsonPartialValue JsonPartialValue
nonNull = prism id (\v -> if isn't _Null v then Right v else Left v)
{-# INLINE nonNull #-}

------------------------------------------------------------------------------
-- Non-primitive traversals
------------------------------------------------------------------------------

class AsPrimitive t => AsValue t where
  _Value :: Prism' t JsonPartialValue

  _Object :: Prism' t (ListMap JsonPartialValue)
  _Object = _Value.prism (JsonPartialObject . fmap (first T.pack) . toList) (\v -> case v of
    JsonPartialObject o -> Right (fromList (fmap (first T.unpack) o))
    _                   -> Left v)
  {-# INLINE _Object #-}

  _Array :: Prism' t [JsonPartialValue]
  _Array = _Value.prism JsonPartialArray (\v -> case v of
    JsonPartialArray a -> Right a
    _                  -> Left v)
  {-# INLINE _Array #-}

instance AsValue JsonPartialValue where
  _Value = id
  {-# INLINE _Value #-}

-- |
-- Like 'ix', but for 'Object' with Text indices. This often has better
-- inference than 'ix' when used with OverloadedStrings.
key :: AsValue t => String -> Traversal' t JsonPartialValue
key i = _Object . ix i
{-# INLINE key #-}

-- | An indexed Traversal into Object properties
members :: AsValue t => IndexedTraversal' String t JsonPartialValue
members = _Object . itraversed
{-# INLINE members #-}

-- | Like 'ix', but for Arrays with Int indexes
nth :: AsValue t => Int -> Traversal' t JsonPartialValue
nth i = _Array . ix i
{-# INLINE nth #-}

-- | An indexed Traversal into Array elements
values :: AsValue t => IndexedTraversal' Int t JsonPartialValue
values = _Array . traversed
{-# INLINE values #-}

-- strictUtf8 :: Iso' String Strict.ByteString
-- strictUtf8 = packed . strictTextUtf8
--
-- strictTextUtf8 :: Iso' Text.Text Strict.ByteString
-- strictTextUtf8 = iso StrictText.encodeUtf8 StrictText.decodeUtf8
--
-- lazyTextUtf8 :: Iso' LazyText.Text Lazy.ByteString
-- lazyTextUtf8 = iso LazyText.encodeUtf8 LazyText.decodeUtf8

------------------------------------------------------------------------------
-- Orphan instances for lens library interop
------------------------------------------------------------------------------

type instance Index JsonPartialValue = String

type instance IxValue JsonPartialValue = JsonPartialValue
instance Ixed JsonPartialValue where
  ix i f (JsonPartialObject o) = (JsonPartialObject . fmap (first T.pack) . toList) <$> ix i f (fromList (fmap (first T.unpack) o))
  ix _ _ v                     = pure v
  {-# INLINE ix #-}

instance Plated JsonPartialValue where
  plate f (JsonPartialObject o) = (JsonPartialObject . fmap (first T.pack) .  toList) <$> traverse f (fromList (fmap (first T.unpack) o))
  plate f (JsonPartialArray a)  = JsonPartialArray <$> traverse f a
  plate _ xs                    = pure xs
  {-# INLINE plate #-}
