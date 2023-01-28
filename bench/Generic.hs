module Generic where

import PeekyBlinders
import PeekyBlinders.Class
import PeekyBlinders.Generic
import Data.Word
import Data.ByteString ( ByteString )

data DSum
  = DSum1 Word8 Word16 Word32 Word64
  | DSum2 Word8 ByteString Word8
  | DSum3 DSum
