module Cea.Pointer.Accessor where

import           Cea.Pointer
import           Data.Proxy
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           GHC.TypeLits

class Accessible (ix :: Nat) a where
  type Accessor ix a
  access :: Proxy ix -> Ptr a -> IO (Ptr (Accessor ix a))

instance (Pointable a, GAccessible ix (Rep a)) => Accessible ix a where
  type Accessor ix a = GAccessor ix (Rep a)

  access :: Proxy ix -> Ptr a -> IO (Ptr x)
  access ix ptr = castPtr <$> gAccess ix (castPtr ptr :: Ptr (Rep a p))
  {-# INLINE access #-}

class GAccessible (ix :: Nat) a where
  type GAccessor ix a
  gAccess :: Proxy ix -> Ptr (a p) -> IO (Ptr (GAccessor ix a))

-- -- TODO: Handle non-primitive types.
instance (GPointable (K1 i a), ConstAccess f (K1 i a), Rep a ~ x, IsPrim a ~ f) => GAccessible 0 (K1 i a) where
  type GAccessor 0 (K1 i a) = ConstAccessor (IsPrim a) (K1 i a)

  gAccess :: Proxy 0 -> Ptr (K1 i a p) -> IO (Ptr (GAccessor 0 (K1 i a)))
  gAccess _ = constAccess (Proxy :: Proxy f)
  {-# INLINE gAccess #-}

instance (GAccessible ix a) => GAccessible ix (M1 i c a) where
  type GAccessor ix (M1 i c a) = GAccessor ix a

  gAccess :: Proxy ix -> Ptr (M1 i c a p) -> IO (Ptr (GAccessor ix (M1 i c a)))
  gAccess ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE gAccess #-}

instance (ProductAccess f ix (a :*: b), IsZero ix ~ f) => GAccessible ix (a :*: b) where
  type GAccessor ix (a :*: b) = ProductAccessor (IsZero ix) ix (a :*: b)

  gAccess :: Proxy ix -> Ptr ((a :*: b) p) -> IO (Ptr (GAccessor ix (a :*: b)))
  gAccess = productAccess (Proxy :: Proxy f)
  {-# INLINE gAccess #-}


--------------------------------------------------------------------------------
-- Helper Types
--------------------------------------------------------------------------------

type family IsZero (n :: Nat) :: Bool where
  IsZero 0 = 'True
  IsZero _ = 'False

class ConstAccess (f :: Bool) a where
  type ConstAccessor f a
  constAccess :: Proxy f -> Ptr (a p) -> IO (Ptr (ConstAccessor f a))

instance ConstAccess 'True (K1 i a) where
  type ConstAccessor 'True (K1 i a) = a

  constAccess :: Proxy 'True -> Ptr (K1 i a p) -> IO (Ptr (ConstAccessor 'True (K1 i a)))
  constAccess _ = pure . castPtr
  {-# INLINE constAccess #-}

instance ConstAccess 'False (K1 i a) where
  type ConstAccessor 'False (K1 i a) = a

  constAccess :: Proxy 'False -> Ptr (K1 i a p) -> IO (Ptr (ConstAccessor 'False (K1 i a)))
  constAccess _ = load . castPtr
  {-# INLINE constAccess #-}

class ProductAccess (f :: Bool) (ix :: Nat) a where
  type ProductAccessor f ix a
  productAccess :: Proxy f -> Proxy ix -> Ptr (a p) -> IO (Ptr (ProductAccessor f ix a))

instance (GAccessible 0 a) => ProductAccess 'True 0 (a :*: b) where
  type ProductAccessor 'True 0 (a :*: b) = GAccessor 0 a

  productAccess :: Proxy 'True -> Proxy 0 -> Ptr ((a :*: b) p) -> IO (Ptr (ProductAccessor 'True 0 (a :*: b)))
  productAccess _ ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE productAccess #-}

instance (GAccessible ix b, ix ~ ix' - 1, KnownNat (ToNat (SLSize f a)), GIsPrim a ~ f, MkSpace f a) => ProductAccess 'False ix' (a :*: b) where
  type ProductAccessor 'False ix' (a :*: b) = GAccessor (ix' - 1) b

  productAccess :: Proxy 'False -> Proxy ix' -> Ptr ((a :*: b) p) -> IO (Ptr (ProductAccessor 'False ix' (a :*: b)))
  productAccess _ ix p = gAccess (Proxy :: Proxy ix) ((castPtr p `plusPtr` slSize (Proxy @f) (Proxy @a)) :: Ptr (b p))
  {-# INLINE productAccess #-}
