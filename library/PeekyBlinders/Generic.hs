{-# LANGUAGE UndecidableInstances #-} -- for generic sum type deriving
{-# LANGUAGE AllowAmbiguousTypes #-}

module PeekyBlinders.Generic where

-- TODO raehik: non-implicit prelude bits below
import Data.Functor
import Control.Applicative
import Control.Monad
import Prelude ( Num(..), Ord(..), otherwise, error )
import Data.Word
import Data.Bits
import Data.Kind

import PeekyBlinders
import PeekyBlinders.Class
import GHC.Generics

-- decodeStaticGeneric, decodeDynamicgeneric
dsg :: (Generic a, GDS (Rep a)) => Static  a
ddg :: (Generic a, GDD (Rep a)) => Dynamic a
dsg = to <$> gds
ddg = to <$> gdd

-- GenericDecodeStatic(gDecodeStatic), GenericDecodeDynamic(gDecodeDynamic)
class GDS f where gds :: Static  (f p)
class GDD f where gdd :: Dynamic (f p)

instance GDS U1 where gds = pure U1
instance GDD U1 where gdd = pure U1

instance DS c => GDS (K1 i c) where gds = K1 <$> ds
instance DD c => GDD (K1 i c) where gdd = K1 <$> dd

instance (GDS l, GDS r) => GDS (l :*: r) where gds = liftA2 (:*:) gds gds
instance (GDD l, GDD r) => GDD (l :*: r) where gdd = liftA2 (:*:) gdd gdd

-- TODO NO SUM TYPES FOR STATIC, NOT EVEN CONSTANT SIZE ONES. SORRY MATE NO
-- INTROSPECTION I DON'T THINK
instance (GDDS (l :+: r), SumSize (l :+: r)) => GDD (l :+: r) where
    gdd = dd @Word8 >>= gddst (sumSize @(l :+: r))

-- from checkGetSum in binary
gddst :: GDDS f => Word8 -> Word8 -> Dynamic (f p)
gddst size code | code < size = gdds code size
                | otherwise   = error "BAD >:(" -- fail "Unknown encoding for constructor"
{-# INLINE gddst #-}

instance GDS f => GDS (M1 i d f) where gds = M1 <$> gds
instance GDD f => GDD (M1 i d f) where gdd = M1 <$> gdd

-- GDecodeDynamicSum(gDecodeDynamicSum)
class GDDS f where gdds :: Word8 -> Word8 -> Dynamic (f p)
instance (GDDS l, GDDS r) => GDDS (l :+: r) where
    gdds !code !size | code < sizeL = L1 <$> gdds code           sizeL
                     | otherwise    = R1 <$> gdds (code - sizeL) sizeR
      where
        -- TODO use unchecked shift? maybe GHC optimizes it out anyway but
        sizeL = size `shiftR` 1
        sizeR = size - sizeL
instance GDD a => GDDS (C1 c a) where gdds _ _ = gdd

--------------------------------------------------------------------------------

class SumSize (f :: Type -> Type) where sumSize :: Num n => n
instance (SumSize l, SumSize r) => SumSize (l :+: r) where
    sumSize = sumSize @l + sumSize @r
instance SumSize (C1 c a) where sumSize = 1
