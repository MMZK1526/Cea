{-# LANGUAGE AllowAmbiguousTypes #-}

module Cea.Pointer.Accessor where

import           Cea.Pointer
import           Data.Proxy
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           GHC.TypeLits

access :: forall ix a. (Pointable a, Accessible ix a) => Ptr a -> IO (Ptr (Accessor ix a))
access = access_ (Proxy :: Proxy ix)
{-# INLINE access #-}

loadAt :: forall ix a. (Pointable a, Accessible ix a, Pointable (Accessor ix a)) => Ptr a -> IO (Accessor ix a)
loadAt ptr = do
  ptr' <- access @ix ptr
  load ptr'
{-# INLINE loadAt #-}

storeAt :: forall ix a. (Pointable a, Accessible ix a, Pointable (Accessor ix a)) => Ptr a -> Accessor ix a -> IO ()
storeAt ptr val = do
  ptr' <- access @ix ptr
  store ptr' val
{-# INLINE storeAt #-}

accesses :: forall ixs a. (Pointable a, Accessibles ixs a) => Ptr a -> IO (Ptr (Accessors ixs a))
accesses = accesses_ (Proxy :: Proxy ixs)
{-# INLINE accesses #-}

loadsAt :: forall ixs a. (Pointable a, Accessibles ixs a, Pointable (Accessors ixs a)) => Ptr a -> IO (Accessors ixs a)
loadsAt ptr = do
  ptr' <- accesses @ixs ptr
  load ptr'
{-# INLINE loadsAt #-}

storesAt :: forall ixs a. (Pointable a, Accessibles ixs a, Pointable (Accessors ixs a)) => Ptr a -> Accessors ixs a -> IO ()
storesAt ptr val = do
  ptr' <- accesses @ixs ptr
  store ptr' val
{-# INLINE storesAt #-}

class Accessible (ix :: Nat) a where
  type Accessor ix a
  access_ :: Proxy ix -> Ptr a -> IO (Ptr (Accessor ix a))

instance (Pointable a, GAccessible ix (Rep a)) => Accessible ix a where
  type Accessor ix a = GAccessor ix (Rep a)

  access_ :: Proxy ix -> Ptr a -> IO (Ptr x)
  access_ ix ptr = castPtr <$> gAccess ix (castPtr ptr :: Ptr (Rep a p))
  {-# INLINE access_ #-}

class GAccessible (ix :: Nat) a where
  type GAccessor ix a
  gAccess :: Proxy ix -> Ptr (a p) -> IO (Ptr (GAccessor ix a))

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

class Accessibles (ixs :: [Nat]) a where
  type Accessors ixs a
  accesses_ :: Proxy ixs -> Ptr a -> IO (Ptr (Accessors ixs a))

instance Accessibles '[] a where
  type Accessors '[] a = a

  accesses_ :: Proxy ('[] :: [Nat]) -> Ptr a -> IO (Ptr (Accessors '[] a))
  accesses_ _ = pure . castPtr
  {-# INLINE accesses_ #-}

instance (Accessible ix a, Accessibles ixs (Accessor ix a)) => Accessibles (ix ': ixs) a where
  type Accessors (ix ': ixs) a = Accessors ixs (Accessor ix a)

  accesses_ :: Proxy (ix ': ixs) -> Ptr a -> IO (Ptr (Accessors (ix ': ixs) a))
  accesses_ _ ptr = do
    ptr' <- access_ (Proxy :: Proxy ix) ptr
    accesses_ (Proxy :: Proxy ixs) ptr'
  {-# INLINE accesses_ #-}


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
