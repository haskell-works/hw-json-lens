
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module HaskellWorks.Data.Xml.Lens where

import           Control.Monad
import           Data.List
import qualified Data.DList                           as DL
import           GHC.Base
import           HaskellWorks.Data.Entry
import           HaskellWorks.Data.Xml.Value
import           HaskellWorks.Data.Entry
import           HaskellWorks.Data.Row

