module Cea.Pointer.Accessor where

import           Cea.Pointer
import           Data.Proxy
import           Foreign.Ptr
import           GHC.Generics
import           GHC.TypeLits

class Accessible (ix :: Nat) a x where
  access :: Proxy ix -> Ptr a -> Ptr x

instance (Pointable a, GAccessible ix (Rep a) (Rep x)) => Accessible ix a x where
  access :: Proxy ix -> Ptr a -> Ptr x
  access ix ptr = castPtr (gAccess ix (castPtr ptr :: Ptr (Rep a p)))
  {-# INLINE access #-}

class GAccessible (ix :: Nat) a x | a -> x where
  gAccess :: Proxy ix -> Ptr (a p) -> Ptr (x p)

-- TODO: Handle non-primitive types.
instance (GPointable (K1 i a), Rep a ~ x) => GAccessible 0 (K1 i a) x where
  gAccess :: Proxy 0 -> Ptr (K1 i a p) -> Ptr (x p)
  gAccess _ = castPtr
  {-# INLINE gAccess #-}

instance (GAccessible ix a x) => GAccessible ix (M1 i c a) x where
  gAccess :: Proxy ix -> Ptr (M1 i c a p) -> Ptr (x p)
  gAccess ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE gAccess #-}

instance (ProductAccess f ix (a :*: b) x, IsZero ix ~ f) => GAccessible ix (a :*: b) x where
  gAccess :: Proxy ix -> Ptr ((a :*: b) p) -> Ptr (x p)
  gAccess = productAccess (Proxy :: Proxy f)
  {-# INLINE gAccess #-}

-- instance (GAccessible 0 a x) => GAccessible 0 (a :*: b) x where
--   gAccess :: Proxy 0 -> Ptr ((a :*: b) p) -> Ptr (x p)
--   gAccess ix p = gAccess ix (castPtr p :: Ptr (a p))
--   {-# INLINE gAccess #-}

-- instance (GAccessible ix b x, ix + 1 ~ ix', KnownNat (ToNat (SLSize f a)), GIsPrim a ~ f, MkSpace f a) => GAccessible ix' (a :*: b) x where
--   gAccess :: Proxy ix' -> Ptr ((a :*: b) p) -> Ptr (x p)
--   gAccess ix p = gAccess (Proxy :: Proxy ix) ((castPtr p `plusPtr` slSize (Proxy @f) (Proxy @a)) :: Ptr (b p))
--   {-# INLINE gAccess #-}


--------------------------------------------------------------------------------
-- Helper Types
--------------------------------------------------------------------------------

type family IsZero (n :: Nat) :: Bool where
  IsZero 0 = 'True
  IsZero _ = 'False

class ProductAccess (f :: Bool) (ix :: Nat) a x | a -> x where
  productAccess :: Proxy f -> Proxy ix -> Ptr (a p) -> Ptr (x p)

instance (GAccessible 0 a x) => ProductAccess 'True 0 (a :*: b) x where
  productAccess :: Proxy 'True -> Proxy 0 -> Ptr ((a :*: b) p) -> Ptr (x p)
  productAccess _ ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE productAccess #-}

instance (GAccessible ix b x, ix + 1 ~ ix', KnownNat (ToNat (SLSize f a)), GIsPrim a ~ f, MkSpace f a) => ProductAccess 'False ix' (a :*: b) x where
  productAccess :: Proxy 'False -> Proxy ix' -> Ptr ((a :*: b) p) -> Ptr (x p)
  productAccess _ ix p = gAccess (Proxy :: Proxy ix) ((castPtr p `plusPtr` slSize (Proxy @f) (Proxy @a)) :: Ptr (b p))
  {-# INLINE productAccess #-}
