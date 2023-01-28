{-# LANGUAGE DerivingStrategies #-}

module Generic where

import Prelude
import PeekyBlinders
import PeekyBlinders.Class
import PeekyBlinders.Generic
import Data.Word
import Data.ByteString ( ByteString )

data DSum
  = DSum0
  | DSum1 Word8 Word16 Word32 Word64
  | DSum2 Word8 ByteString Word8
  | DSum3 DSum
    deriving stock (Generic, Eq, Show)

-- no DD because bytestring non-static
instance DD DSum where dd = ddg
