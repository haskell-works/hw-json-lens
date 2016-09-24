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

import           Control.Applicative
import           Control.Lens
import           Data.Data
import           Data.Scientific                     (Scientific)
import qualified Data.Scientific                     as Scientific
import           GHC.Base
import           HaskellWorks.Data.Json.PartialValue as J
import           HaskellWorks.Data.ListMap           (ListMap, fromList, toList)
import           Prelude                             hiding (null)

------------------------------------------------------------------------------
-- Scientific prisms
------------------------------------------------------------------------------

class AsNumber t where
  -- |
  -- >>> "[1, \"x\"]" ^? nth 0 . _Number
  -- Just 1.0
  --
  -- >>> "[1, \"x\"]" ^? nth 1 . _Number
  -- Nothing
  _Number :: Prism' t Scientific
#ifndef HLINT
  default _Number :: AsPrimitive t => Prism' t Scientific
  _Number = _Primitive._Number
  {-# INLINE _Number #-}
#endif

  -- |
  -- Prism into an 'Double' over a 'Value', 'Primitive' or 'Scientific'
  --
  -- >>> "[10.2]" ^? nth 0 . _Double
  -- Just 10.2
  _Double :: Prism' t Double
  _Double = _Number.iso Scientific.toRealFloat realToFrac
  {-# INLINE _Double #-}

  -- |
  -- Prism into an 'Integer' over a 'Value', 'Primitive' or 'Scientific'
  --
  -- >>> "[10]" ^? nth 0 . _Integer
  -- Just 10
  --
  -- >>> "[10.5]" ^? nth 0 . _Integer
  -- Just 10
  --
  -- >>> "42" ^? _Integer
  -- Just 42
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

-- We can implement these once jw-json declared FromJSON/ToJSON classes

-- instance AsNumber Strict.ByteString
-- instance AsNumber Lazy.ByteString
-- instance AsNumber Text
-- instance AsNumber LazyText.Text
-- instance AsNumber String

------------------------------------------------------------------------------
-- Conversion Prisms
------------------------------------------------------------------------------

-- | Access Integer 'Value's as Integrals.
--
-- >>> "[10]" ^? nth 0 . _Integral
-- Just 10
--
-- >>> "[10.5]" ^? nth 0 . _Integral
-- Just 10
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
  -- |
  -- >>> "[1, \"x\", null, true, false]" ^? nth 0 . _Primitive
  -- Just (NumberPrim 1.0)
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 1 . _Primitive
  -- Just (StringPrim "x")
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 2 . _Primitive
  -- Just NullPrim
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 3 . _Primitive
  -- Just (BoolPrim True)
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 4 . _Primitive
  -- Just (BoolPrim False)
  _Primitive :: Prism' t Primitive
#ifndef HLINT
  default _Primitive :: AsValue t => Prism' t Primitive
  _Primitive = _Value._Primitive
  {-# INLINE _Primitive #-}
#endif

  -- |
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "a" . _String
  -- Just "xyz"
  --
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "b" . _String
  -- Nothing
  --
  -- >>> _Object._Wrapped # [("key" :: Text, _String # "value")] :: String
  -- "{\"key\":\"value\"}"
  _String :: Prism' t String
  _String = _Primitive.prism StringPrim (\v -> case v of StringPrim s -> Right s; _ -> Left v)
  {-# INLINE _String #-}

  -- |
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "b" . _Bool
  -- Just True
  --
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "a" . _Bool
  -- Nothing
  --
  -- >>> _Bool # True :: String
  -- "true"
  --
  -- >>> _Bool # False :: String
  -- "false"
  _Bool :: Prism' t Bool
  _Bool = _Primitive.prism BoolPrim (\v -> case v of BoolPrim b -> Right b; _ -> Left v)
  {-# INLINE _Bool #-}

  -- |
  -- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "b" . _Null
  -- Just ()
  --
  -- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "a" . _Null
  -- Nothing
  --
  -- >>> _Null # () :: String
  -- "null"
  _Null :: Prism' t ()
  _Null = _Primitive.prism (const NullPrim) (\v -> case v of NullPrim -> Right (); _ -> Left v)
  {-# INLINE _Null #-}


instance AsPrimitive JsonPartialValue where
  _Primitive = prism fromPrim toPrim
    where
      toPrim (JsonPartialString s) = Right $ StringPrim s
      toPrim (JsonPartialNumber n) = Right $ NumberPrim (Scientific.fromFloatDigits n)
      toPrim (JsonPartialBool b)   = Right $ BoolPrim b
      toPrim JsonPartialNull       = Right NullPrim
      toPrim v          = Left v
      {-# INLINE toPrim #-}
      fromPrim (StringPrim s) = JsonPartialString s
      fromPrim (NumberPrim n) = JsonPartialNumber (realToFrac n)
      fromPrim (BoolPrim b)   = JsonPartialBool b
      fromPrim NullPrim       = JsonPartialNull
      {-# INLINE fromPrim #-}
  {-# INLINE _Primitive #-}
  _String = prism JsonPartialString $ \v -> case v of JsonPartialString s -> Right s; _ -> Left v
  {-# INLINE _String #-}
  _Bool = prism JsonPartialBool (\v -> case v of JsonPartialBool b -> Right b; _ -> Left v)
  {-# INLINE _Bool #-}
  _Null = prism (const JsonPartialNull) (\v -> case v of JsonPartialNull -> Right (); _ -> Left v)
  {-# INLINE _Null #-}

-- instance AsPrimitive Strict.ByteString
-- instance AsPrimitive Lazy.ByteString
-- instance AsPrimitive Text.Text
-- instance AsPrimitive LazyText.Text
-- instance AsPrimitive String

instance AsPrimitive Primitive where
  _Primitive = id
  {-# INLINE _Primitive #-}

-- | Prism into non-'Null' values
--
-- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "a" . nonNull
-- Just (String "xyz")
--
-- >>> "{\"a\": {}, \"b\": null}" ^? key "a" . nonNull
-- Just (Object (fromList []))
--
-- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "b" . nonNull
-- Nothing
nonNull :: Prism' JsonPartialValue JsonPartialValue
nonNull = prism id (\v -> if isn't _Null v then Right v else Left v)
{-# INLINE nonNull #-}

------------------------------------------------------------------------------
-- Non-primitive traversals
------------------------------------------------------------------------------

class AsPrimitive t => AsValue t where
  -- |
  -- >>> preview _Value "[1,2,3]" == Just (Array (Vector.fromList [Number 1.0,Number 2.0,Number 3.0]))
  -- True
  _Value :: Prism' t JsonPartialValue

  -- |
  -- >>> "{\"a\": {}, \"b\": null}" ^? key "a" . _Object
  -- Just (fromList [])
  --
  -- >>> "{\"a\": {}, \"b\": null}" ^? key "b" . _Object
  -- Nothing
  --
  -- >>> _Object._Wrapped # [("key" :: String, _String # "value")] :: String
  -- "{\"key\":\"value\"}"
  _Object :: Prism' t (ListMap JsonPartialValue)
  _Object = _Value.prism (JsonPartialObject . toList) (\v -> case v of
    JsonPartialObject o -> Right (fromList o)
    _                   -> Left v)
  {-# INLINE _Object #-}

  -- |
  -- >>> preview _Array "[1,2,3]" == Just (Vector.fromList [Number 1.0,Number 2.0,Number 3.0])
  -- True
  _Array :: Prism' t [JsonPartialValue]
  _Array = _Value.prism JsonPartialArray (\v -> case v of
    JsonPartialArray a -> Right a
    _                  -> Left v)
  {-# INLINE _Array #-}

instance AsValue JsonPartialValue where
  _Value = id
  {-# INLINE _Value #-}

-- instance AsValue Strict.ByteString where
--   _Value = _JSON
--   {-# INLINE _Value #-}
--
-- instance AsValue Lazy.ByteString where
--   _Value = _JSON
--   {-# INLINE _Value #-}
--
-- instance AsValue String where
--   _Value = strictUtf8._JSON
--   {-# INLINE _Value #-}
--
-- instance AsValue Text where
--   _Value = strictTextUtf8._JSON
--   {-# INLINE _Value #-}
--
-- instance AsValue LazyText.Text where
--   _Value = lazyTextUtf8._JSON
--   {-# INLINE _Value #-}

-- |
-- Like 'ix', but for 'Object' with Text indices. This often has better
-- inference than 'ix' when used with OverloadedStrings.
--
-- >>> "{\"a\": 100, \"b\": 200}" ^? key "a"
-- Just (Number 100.0)
--
-- >>> "[1,2,3]" ^? key "a"
-- Nothing
key :: AsValue t => String -> Traversal' t JsonPartialValue
key i = _Object . ix i
{-# INLINE key #-}

-- | An indexed Traversal into Object properties
--
-- >>> "{\"a\": 4, \"b\": 7}" ^@.. members
-- [("a",Number 4.0),("b",Number 7.0)]
--
-- >>> "{\"a\": 4, \"b\": 7}" & members . _Number *~ 10
-- "{\"a\":40,\"b\":70}"
members :: AsValue t => IndexedTraversal' String t JsonPartialValue
members = _Object . itraversed
{-# INLINE members #-}

-- | Like 'ix', but for Arrays with Int indexes
--
-- >>> "[1,2,3]" ^? nth 1
-- Just (Number 2.0)
--
-- >>> "\"a\": 100, \"b\": 200}" ^? nth 1
-- Nothing
--
-- >>> "[1,2,3]" & nth 1 .~ Number 20
-- "[1,20,3]"
nth :: AsValue t => Int -> Traversal' t JsonPartialValue
nth i = _Array . ix i
{-# INLINE nth #-}

-- | An indexed Traversal into Array elements
--
-- >>> "[1,2,3]" ^.. values
-- [Number 1.0,Number 2.0,Number 3.0]
--
-- >>> "[1,2,3]" & values . _Number *~ 10
-- "[10,20,30]"
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

-- class AsJSON t where
--   -- | '_JSON' is a 'Prism' from something containing JSON to something encoded in that structure
--   _JSON :: (FromJSON a, ToJSON a) => Prism' t a
--
-- instance AsJSON Strict.ByteString where
--   _JSON = lazy._JSON
--   {-# INLINE _JSON #-}
--
-- instance AsJSON Lazy.ByteString where
--   _JSON = prism' encode decodeValue
--     where
--       decodeValue :: (FromJSON a) => Lazy.ByteString -> Maybe a
--       decodeValue s = maybeResult (parse value s) >>= \x -> case fromJSON x of
--         Success v -> Just v
--         _         -> Nothing
--   {-# INLINE _JSON #-}
--
-- instance AsJSON String where
--   _JSON = strictUtf8._JSON
--   {-# INLINE _JSON #-}
--
-- instance AsJSON Text where
--   _JSON = strictTextUtf8._JSON
--   {-# INLINE _JSON #-}
--
-- instance AsJSON LazyText.Text where
--   _JSON = lazyTextUtf8._JSON
--   {-# INLINE _JSON #-}
--
-- instance AsJSON Value where
--   _JSON = prism toJSON $ \x -> case fromJSON x of
--     Success y -> Right y;
--     _         -> Left x
--   {-# INLINE _JSON #-}

------------------------------------------------------------------------------
-- Some additional tests for prismhood; see https://github.com/ekmett/lens/issues/439.
------------------------------------------------------------------------------

-- $LazyByteStringTests
-- >>> "42" ^? (_JSON :: Prism' Lazy.ByteString Value)
-- Just (Number 42.0)
--
-- >>> preview (_Integer :: Prism' Lazy.ByteString Integer) "42"
-- Just 42
--
-- >>> Lazy.unpack (review (_Integer :: Prism' Lazy.ByteString Integer) 42)
-- "42"

-- $StrictByteStringTests
-- >>> "42" ^? (_JSON :: Prism' Strict.ByteString Value)
-- Just (Number 42.0)
--
-- >>> preview (_Integer :: Prism' Strict.ByteString Integer) "42"
-- Just 42
--
-- >>> Strict.Char8.unpack (review (_Integer :: Prism' Strict.ByteString Integer) 42)
-- "42"

-- $StringTests
-- >>> "42" ^? (_JSON :: Prism' String Value)
-- Just (Number 42.0)
--
-- >>> preview (_Integer :: Prism' String Integer) "42"
-- Just 42
--
-- >>> review (_Integer :: Prism' String Integer) 42
-- "42"

------------------------------------------------------------------------------
-- Orphan instances for lens library interop
------------------------------------------------------------------------------

type instance Index JsonPartialValue = String

type instance IxValue JsonPartialValue = JsonPartialValue
instance Ixed JsonPartialValue where
  ix i f (JsonPartialObject o) = (JsonPartialObject . toList) <$> ix i f (fromList o)
  ix _ _ v          = pure v
  {-# INLINE ix #-}

instance Plated JsonPartialValue where
  plate f (JsonPartialObject o) = (JsonPartialObject . toList) <$> traverse f (fromList o)
  plate f (JsonPartialArray a) = JsonPartialArray <$> traverse f a
  plate _ xs = pure xs
  {-# INLINE plate #-}
