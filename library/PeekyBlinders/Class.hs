{-# LANGUAGE DerivingVia #-}

module PeekyBlinders.Class where

-- TODO non-implicit prelude bits
import Data.Functor
import Prelude ( fromIntegral )

import PeekyBlinders

import Data.Word
import Data.Int
import Data.ByteString ( ByteString )

import qualified Ptr.IO

-- DecodeStatic(decodeStatic), DecodeDynamic(decodeDynamic)
class DS a where ds :: Static  a
class DD a where dd :: Dynamic a

-- pretty derivingvia wrapper :) good boy :)
newtype Statically a = Statically { unStatically :: a }
instance DS a => DD (Statically a) where dd = Statically <$> statically ds

instance DS Word8 where ds = unsignedInt1
deriving via Statically Word8 instance DD Word8

-- TODO use host endianness and word size...

instance DS Word16 where ds = leUnsignedInt2
deriving via Statically Word16 instance DD Word16

instance DS Word32 where ds = leUnsignedInt4
deriving via Statically Word32 instance DD Word32

instance DS Word64 where ds = leUnsignedInt8
deriving via Statically Word64 instance DD Word64

instance DS Int where ds = fromIntegral <$> leSignedInt8
deriving via Statically Int instance DD Int

-- TODO we have to decide nullterm (C) or length prefix (Pascal). pretty sure
-- latter is faster, if a bit bulkier for short strings.
instance DD ByteString where
    -- TODO raehik: Bad, probs buggy and certainly uber slow. sorry
    dd = do
        x <- dd @Int
        forceSize x remainderAsByteString
