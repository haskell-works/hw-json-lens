{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.NthSpec (spec) where

import Control.Lens
import GHC.Base
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.Lens
import HaskellWorks.Data.Json.PartialValue
import Prelude                                      hiding (null)
import Test.Hspec

import qualified Data.ByteString                             as Strict
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

j :: Strict.ByteString -> JsonPartialValue
j = jsonPartialJsonValueAt . jsonPartialIndexAt . JCF.fromByteStringViaBlanking

spec :: Spec
spec = describe "Nth Spec" $ do
  let json = j "[1.5, \"x\", null, true, false]"
  it "nth" $ do
    json ^? nth 0 . _Number   `shouldBe` Just 1.5
    json ^? nth 0 . _Double   `shouldBe` Just 1.5
    json ^? nth 0 . _Integer  `shouldBe` Just 1
    json ^? nth 0 . _String   `shouldBe` Nothing

  it "nth with string" $ do
    json ^? nth 1 . _String   `shouldBe` Just "x"
    json ^? nth 1 . _Double   `shouldBe` Nothing

  it "nth out of bound" $ do
    json ^? nth 8 . _Double   `shouldBe` Nothing
    json ^? nth 22 . _String  `shouldBe` Nothing

  it "nth with null" $ do
    json ^? nth 2 . _Null `shouldBe` Just ()
    json ^? nth 3 . _Null `shouldBe` Nothing

  it "nth with bool" $ do
    json ^? nth 3 . _Bool `shouldBe` Just True
    json ^? nth 4 . _Bool `shouldBe` Just False
    json ^? nth 2 . _Bool `shouldBe` Nothing

  it "nth with primitive" $ do
    json ^? nth 0 . _Primitive `shouldBe` Just (NumberPrim 1.5)
    json ^? nth 1 . _Primitive `shouldBe` Just (StringPrim "x")
    json ^? nth 2 . _Primitive `shouldBe` Just NullPrim
    json ^? nth 3 . _Primitive `shouldBe` Just (BoolPrim True)
    json ^? nth 4 . _Primitive `shouldBe` Just (BoolPrim False)
