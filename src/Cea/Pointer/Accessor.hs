{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

-- | This module provides functions to access the fields of a @Pointable@
-- pointer. It allows access to the pointer to any nested field in the data
-- structure as well as storing/loading the value of the field.
module Cea.Pointer.Accessor
  ( access
  , accesses
  , loadAt
  , loadsAt
  , storeAt
  , storesAt
  , Accessible (Accessor)
  ) where

import           Cea.Pointer.Internal
import           Cea.Utils.Symbol2Nat
import           Data.Kind
import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.Ord
import           Foreign.Ptr
import           GHC.Generics
import           GHC.TypeLits

-- | Acquire a pointer to the field at the given index. The index starts from 0.
--
-- > do
-- >   ptr <- make ((), 'a')
-- >   ptr' <- access @1 ptr -- ptr' :: Ptr Char
-- >   load ptr' -- 'a'
access :: forall ix a. (Pointable a, Accessible ix a)
       => Ptr a -> IO (Ptr (Accessor ix a))
access = access_ (Proxy :: Proxy ix)
{-# INLINE access #-}

-- | Load the value of the field at the given index. The index starts from 0.
--
-- > do
-- >   ptr <- make ((), 'a')
-- >   loadAt @1 ptr -- 'a'
loadAt :: forall ix a. (Pointable a, Accessible ix a, Pointable (Accessor ix a))
       => Ptr a -> IO (Accessor ix a)
loadAt ptr = do
  ptr' <- access @ix ptr
  load ptr'
{-# INLINE loadAt #-}

-- | Store the value of the field at the given index. The index starts from 0.
--
-- > do
-- >   ptr <- make ((), 'a')
-- >   storeAt @1 ptr 'b'
-- >   loadAt @1 ptr -- 'b'
storeAt :: forall ix a
         . (Pointable a, Accessible ix a, Pointable (Accessor ix a))
        => Ptr a -> Accessor ix a -> IO ()
storeAt ptr val = do
  ptr' <- access @ix ptr
  store ptr' val
{-# INLINE storeAt #-}

-- | Similar to @access@ but for nested fields. It takes a type-level list of
-- indices and returns a pointer to the field at the given path.
--
-- > do
-- >   ptr  <- make (((), 'a'), 2 :: Int)
-- >   ptr' <- accesses @'[0, 1] ptr -- ptr' :: Ptr Char
-- >   load ptr' -- 'a'
accesses :: forall ixs a. (Pointable a, Accessibles ixs a)
         => Ptr a -> IO (Ptr (Accessors ixs a))
accesses = accesses_ (Proxy :: Proxy ixs)
{-# INLINE accesses #-}

-- | Similar to @loadAt@ but for nested fields. It takes a type-level list of
-- indices and returns the value of the field at the given path.
--
-- > do
-- >   ptr <- make (((), 'a'), 2 :: Int)
-- >   loadsAt @'[0, 1] ptr -- 'a'
loadsAt :: forall ixs a
         . (Pointable a, Accessibles ixs a, Pointable (Accessors ixs a))
        => Ptr a -> IO (Accessors ixs a)
loadsAt ptr = do
  ptr' <- accesses @ixs ptr
  load ptr'
{-# INLINE loadsAt #-}

-- | Similar to @storeAt@ but for nested fields. It takes a type-level list of
-- indices and stores the value of the field at the given path.
--
-- > do
-- >   ptr <- make (((), 'a'), 2 :: Int)
-- >   storesAt @'[0, 1] ptr 'b'
-- >   loadsAt @'[0, 1] ptr -- 'b'
storesAt :: forall ixs a
          . (Pointable a, Accessibles ixs a, Pointable (Accessors ixs a))
         => Ptr a -> Accessors ixs a -> IO ()
storesAt ptr val = do
  ptr' <- accesses @ixs ptr
  store ptr' val
{-# INLINE storesAt #-}

-- | A type class that supports access to each field.
--
-- It is not required to implement this type class manually. Instead, any
-- data type that derives @Pointable@ via @WithPointable@ will automatically
-- derive @Accessible@.
class Accessible ix a where
  type Accessor ix a

  access_ :: Proxy ix -> Ptr a -> IO (Ptr (Accessor ix a))


--------------------------------------------------------------------------------
-- Generic Derivation Wrapper
--------------------------------------------------------------------------------

-- The instance is derived from @GAccessible@, a generic version of
-- @Accessible@.
instance (Pointable a, GAccessible ix (Rep a)) => Accessible ix a where
  -- | The type for the field at the given index.
  type Accessor ix a = GAccessor ix (Rep a)

  -- | Similar to "access" but requires the usage of @Proxy@.
  access_ :: Proxy ix -> Ptr a -> IO (Ptr x)
  access_ ix ptr = castPtr <$> gAccess ix (castPtr ptr :: Ptr (Rep a p))
  {-# INLINE access_ #-}


--------------------------------------------------------------------------------
-- Generic Accessible
--------------------------------------------------------------------------------

class GAccessible ix (a :: Type -> Type) where
  type GAccessor ix a

  gAccess :: Proxy ix -> Ptr (a p) -> IO (Ptr (GAccessor ix a))

instance ( GPointable (K1 i a)
         , ConstAccess f (K1 @Type i a)
         , IsDirect a ~ f )
  => GAccessible 0 (K1 i a) where
    type GAccessor 0 (K1 i a) = ConstAccessor (IsDirect a) (K1 i a)

    gAccess :: Proxy 0
            -> Ptr (K1 @Type i a p)
            -> IO (Ptr (GAccessor 0 (K1 i a)))
    gAccess _ = constAccess (Proxy :: Proxy f)
    {-# INLINE gAccess #-}

instance (GAccessible (ix :: Nat) a) => GAccessible ix (M1 i c a) where
  type GAccessor ix (M1 i c a) = GAccessor ix a

  gAccess :: Proxy ix -> Ptr (M1 i c a p) -> IO (Ptr (GAccessor ix (M1 i c a)))
  gAccess ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE gAccess #-}

instance (GAccessible (ix :: Symbol) a) => GAccessible ix (D1 c a) where
  type GAccessor ix (D1 c a) = GAccessor ix a

  gAccess :: Proxy ix -> Ptr (D1 c a p) -> IO (Ptr (GAccessor ix (D1 c a)))
  gAccess ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE gAccess #-}

instance (GAccessible (ix :: Symbol) a) => GAccessible ix (C1 c a) where
  type GAccessor ix (C1 c a) = GAccessor ix a

  gAccess :: Proxy ix -> Ptr (C1 c a p) -> IO (Ptr (GAccessor ix (D1 c a)))
  gAccess ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE gAccess #-}

instance (GAccessible (ix :: Symbol) a)
  => GAccessible ix (S1 ('MetaSel ('Just ix) su ss ds) a) where
    type GAccessor ix (S1 ('MetaSel ('Just ix) su ss ds) a) = GAccessor ix a

    gAccess :: Proxy ix -> Ptr (M1 i c a p) -> IO (Ptr (GAccessor ix (D1 c a)))
    gAccess ix p = gAccess ix (castPtr p :: Ptr (a p))
    {-# INLINE gAccess #-}

instance ( GPointable (K1 i a)
         , ConstAccess f (K1 @Type i a)
         , IsDirect a ~ f )
  => GAccessible (ix :: Symbol) (K1 @Type i a) where
    type GAccessor ix (K1 @Type i a) = GAccessor 0 (K1 @Type i a)

    gAccess :: Proxy ix
            -> Ptr (K1 @Type i a p)
            -> IO (Ptr (GAccessor ix (K1 @Type i a)))
    gAccess _ = gAccess (Proxy :: Proxy 0)
    {-# INLINE gAccess #-}

instance ( ProductAccess f (ix :: Nat) (a :*: b)
         , LessThan ix (GLength a) ~ f )
  => GAccessible ix (a :*: b) where
    type GAccessor ix (a :*: b) =
      ProductAccessor (LessThan ix (GLength a)) ix (a :*: b)

    gAccess :: Proxy ix -> Ptr ((a :*: b) p)
            -> IO (Ptr (GAccessor ix (a :*: b)))
    gAccess = productAccess (Proxy :: Proxy f)
    {-# INLINE gAccess #-}

instance ( ProductAccess f (ix :: Symbol) (a :*: b)
         , HasField ix a ~ f )
  => GAccessible ix (a :*: b) where
    type GAccessor ix (a :*: b) =
      ProductAccessor (HasField ix a) ix (a :*: b)

    gAccess :: Proxy ix -> Ptr ((a :*: b) p)
            -> IO (Ptr (GAccessor ix (a :*: b)))
    gAccess = productAccess (Proxy :: Proxy f)
    {-# INLINE gAccess #-}


--------------------------------------------------------------------------------
-- Accessibles
--------------------------------------------------------------------------------

class Accessibles (ixs :: [k]) a where
  type Accessors ixs a
  accesses_ :: Proxy ixs -> Ptr a -> IO (Ptr (Accessors ixs a))

instance Accessibles '[] a where
  type Accessors '[] a = a

  accesses_ :: Proxy ('[] :: [k]) -> Ptr a -> IO (Ptr (Accessors '[] a))
  accesses_ _ = pure . castPtr
  {-# INLINE accesses_ #-}

instance (Accessible ix a, Accessibles ixs (Accessor ix a))
  => Accessibles (ix ': ixs) a where
    type Accessors (ix ': ixs) a = Accessors ixs (Accessor ix a)

    accesses_ :: Proxy (ix ': ixs) -> Ptr a
              -> IO (Ptr (Accessors (ix ': ixs) a))
    accesses_ _ ptr = do
      ptr' <- access_ (Proxy :: Proxy ix) ptr
      accesses_ (Proxy :: Proxy ixs) ptr'
    {-# INLINE accesses_ #-}


--------------------------------------------------------------------------------
-- GHasLength
--------------------------------------------------------------------------------

class GHasLength (a :: Type -> Type) where
  type GLength a :: Nat

instance GHasLength (K1 i a) where
  type GLength (K1 i a) = 1

instance GHasLength a => GHasLength (M1 i c a) where
  type GLength (M1 i c a) = GLength a

instance (GHasLength a, GHasLength b) => GHasLength (a :*: b) where
  type GLength (a :*: b) = GLength a + GLength b


--------------------------------------------------------------------------------
-- GHasField
--------------------------------------------------------------------------------

class GHasField (ix :: Symbol) (a :: Type -> Type) where
  type HasField ix a :: Bool

instance (GHasField ix a) => GHasField ix (D1 c a) where
  type HasField ix (D1 c a) = HasField ix a

instance (GHasField ix a) => GHasField ix (C1 c a) where
  type HasField ix (C1 c a) = HasField ix a

instance GHasField ix (S1 ('MetaSel Nothing su ss ds) a) where
  type HasField ix (S1 ('MetaSel Nothing su ss ds) a) = 'False

instance GHasField ix (S1 ('MetaSel (Just ix') su ss ds) a) where
  type HasField ix (S1 ('MetaSel (Just ix') su ss ds) a) = Same ix ix'

instance (GHasField ix a, GHasField ix b) => GHasField ix (a :*: b) where
  type HasField ix (a :*: b) = HasField ix a || HasField ix b


--------------------------------------------------------------------------------
-- Helper Types
--------------------------------------------------------------------------------

type family Same (s :: Symbol) (s' :: Symbol) :: Bool where
  Same s s = 'True
  Same _ _ = 'False

type family LessThan (n :: Nat) (n' :: Nat) :: Bool where
  LessThan n n' = IsLessThan (Compare n n')

type family IsLessThan (o :: Ordering) :: Bool where
  IsLessThan 'LT = 'True
  IsLessThan _   = 'False

type family IsZero (n :: Nat) :: Bool where
  IsZero 0 = 'True
  IsZero _ = 'False

class ConstAccess (f :: Bool) (a :: Type -> Type) where
  type ConstAccessor f a

  constAccess :: Proxy f -> Ptr (a p) -> IO (Ptr (ConstAccessor f a))

instance ConstAccess 'True (K1 i a) where
  type ConstAccessor 'True (K1 i a) = a

  constAccess :: Proxy 'True -> Ptr (K1 i a p)
              -> IO (Ptr (ConstAccessor 'True (K1 i a)))
  constAccess _ = pure . castPtr
  {-# INLINE constAccess #-}

instance ConstAccess 'False (K1 i a) where
  type ConstAccessor 'False (K1 i a) = a

  constAccess :: Proxy 'False -> Ptr (K1 i a p)
              -> IO (Ptr (ConstAccessor 'False (K1 i a)))
  constAccess _ = load . castPtr
  {-# INLINE constAccess #-}

class ProductAccess (f :: Bool) ix (a :: Type -> Type) where
  type ProductAccessor f ix a

  productAccess :: Proxy f -> Proxy ix -> Ptr (a p)
                -> IO (Ptr (ProductAccessor f ix a))

instance (GAccessible (ix :: Nat) a) => ProductAccess 'True ix (a :*: b) where
  type ProductAccessor 'True ix (a :*: b) = GAccessor ix a

  productAccess :: Proxy 'True -> Proxy ix -> Ptr ((a :*: b) p)
                -> IO (Ptr (ProductAccessor 'True ix (a :*: b)))
  productAccess _ ix p = gAccess ix (castPtr p :: Ptr (a p))
  {-# INLINE productAccess #-}

instance ( GAccessible (ix :: Nat) b
         , KnownNat (ToNat (SLSize f a))
         , ix ~ ix' - GLength a, GIsDirect a ~ f
         , HasSLSize f a )
  => ProductAccess 'False ix' (a :*: b) where
    type ProductAccessor 'False ix' (a :*: b) = GAccessor (ix' - GLength a) b

    productAccess :: Proxy 'False -> Proxy ix' -> Ptr ((a :*: b) p)
                  -> IO (Ptr (ProductAccessor 'False ix' (a :*: b)))
    productAccess _ ix p = gAccess (Proxy :: Proxy ix)
      ((castPtr p `plusPtr` slSize (Proxy @f) (Proxy @a)) :: Ptr (b p))
    {-# INLINE productAccess #-}

instance (GAccessible (ix :: Symbol) a)
  => ProductAccess 'True ix (a :*: b) where
    type ProductAccessor 'True ix (a :*: b) = GAccessor ix a

    productAccess :: Proxy 'True -> Proxy ix -> Ptr ((a :*: b) p)
                  -> IO (Ptr (ProductAccessor 'True ix (a :*: b)))
    productAccess _ ix p = gAccess ix (castPtr p :: Ptr (a p))
    {-# INLINE productAccess #-}

instance ( GAccessible (ix :: Symbol) b
         , KnownNat (ToNat (SLSize f a))
         , GIsDirect a ~ f
         , HasSLSize f a )
  => ProductAccess 'False ix (a :*: b) where
    type ProductAccessor 'False ix (a :*: b) = GAccessor ix b

    productAccess :: Proxy 'False -> Proxy ix -> Ptr ((a :*: b) p)
                  -> IO (Ptr (ProductAccessor 'False ix (a :*: b)))
    productAccess _ ix p = gAccess (Proxy :: Proxy ix)
      ((castPtr p `plusPtr` slSize (Proxy @f) (Proxy @a)) :: Ptr (b p))
    {-# INLINE productAccess #-}
