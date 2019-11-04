{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.LensSpec (spec) where

import Control.Lens
import GHC.Base
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.Lens
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.ListMap                    (fromList)
import Prelude                                      hiding (null)
import Test.Hspec

import qualified Data.ByteString                             as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

j :: BS.ByteString -> JsonPartialValue
j bs = jsonPartialJsonValueAt (jsonPartialIndexAt (JCF.fromByteStringViaBlanking bs))

spec :: Spec
spec = describe "Nth Spec" $ do
  let json = j "{\"d\": 1.5, \"s\": \"xyz\", \"b\": true, \"n\": null, \"o\": {}}"

  it "key" $ do
    json ^? key "d"             `shouldBe` Just (JsonPartialNumber 1.5)
    j "[1,2,3]" ^? key "s"      `shouldBe` Nothing

  it "key composed" $ do
    json ^? key "d" . _Number   `shouldBe` Just 1.5
    json ^? key "s" . _String   `shouldBe` Just "xyz"
    json ^? key "b" . _Bool     `shouldBe` Just True
    json ^? key "n" . _Null     `shouldBe` Just ()
    json ^? key "b" . _Null     `shouldBe` Nothing

  it "nonNull" $ do
    json ^? key "s" . nonNull   `shouldBe` Just (JsonPartialString "xyz")
    json ^? key "o" . nonNull   `shouldBe` Just (JsonPartialObject [])
    json ^? key "n" . nonNull   `shouldBe` Nothing

  it "preview" $ do
    preview _Value (j "[1,2,3]") `shouldBe` Just (JsonPartialArray [JsonPartialNumber 1.0,JsonPartialNumber 2.0,JsonPartialNumber 3.0])

  it "object" $ do
    json ^? key "o" . _Object   `shouldBe` Just (fromList [])
    json ^? key "n" . _Object   `shouldBe` Nothing

  it "members" $ do
    fromList (json ^@.. members) `shouldBe` fromList [("d", JsonPartialNumber 1.5), ("s", JsonPartialString "xyz"), ("b", JsonPartialBool True), ("n", JsonPartialNull), ("o", JsonPartialObject [])]
    (json & members . _Number *~10) ^? key "d" `shouldBe` Just (JsonPartialNumber 15)

  it "values" $ do
    j "[1,2,3]" ^.. values                  `shouldBe` [JsonPartialNumber 1, JsonPartialNumber 2, JsonPartialNumber 3]
    (j "[1,2,3]" & values . _Number *~ 10)  `shouldBe` JsonPartialArray [JsonPartialNumber 10, JsonPartialNumber 20, JsonPartialNumber 30]
