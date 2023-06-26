{-# LANGUAGE CPP #-}

#ifndef CEA_DEBUG
-- #define CEA_DEBUG
#endif

-- | This module contains the type class @Pointable@, which enables the generic
-- marshalling between C-pointer and Haskell data types. It implements
-- @Pointable@ for the "primitive" types, and provides a generic implementation
-- for most other types (TODO: Sum type not supported yet).
--
-- The only exposed functions are the methods of the @Pointable@ type class.
-- Others are implementation details that can be changed without prior notice.
module Cea.Pointer.Internal where

import           Data.Char
import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Word
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.Generics
import           GHC.TypeLits

-- | A type class that supports marshalling from and to C-pointers.
--
-- This type class rarely requires manual implementation, and the common way of
-- declaring an instance of it is via the helper @WithPointable@ (using the 
-- extensions @DeriveGeneric@ and @DerivingVia@).
--
-- For example, the following code declares an instance of @Pointable@ for the
-- type @Foo@:
--
-- > data Foo = Foo Int8 Int16 Int32 Int64
-- >   deriving (Generic)
-- >   deriving (Pointable) via WithPointable Foo
class (Val (SizeOf a), KnownBool (IsDirect a)) => Pointable a where
  -- | The size of the type in bytes.
  type SizeOf a :: MyNat

  -- | Whether the type is a direct type. If the type is indirect, it will be
  -- stored via a layer of pointer indirection.
  --
  -- By default, it is @'True@ for all concrete instances in this module, and
  -- @'False@ for all user-defined types.
  type IsDirect a :: Bool

  -- | Term-level version of @SizeOf@.
  size :: a -> Int
  size _ = fromIntegral $ val (Proxy :: Proxy (SizeOf a))

  -- | Create a new pointer and store the value in it, creating pointers for
  -- the fields if necessary.
  make :: a -> IO (Ptr a)
  make a = do
    ptr <- mallocBytes (size a)
    makeInner ptr a
    pure $ castPtr ptr
  {-# INLINE make #-}

  -- | Store the value in the given pointer, creating pointers for the fields
  -- if necessary.
  makeInner :: Ptr a -> a -> IO ()

  -- | Load the value from the pointer.
  load :: Ptr a -> IO a

  -- | Store the value in the pointer.
  --
  -- The signature of this function is the same as @makeInner@, but the
  -- difference is that @store@ never creates any new pointers and always assume
  -- that all nested pointers are already allocated.
  store :: Ptr a -> a -> IO ()

  -- | Free the pointer recursively.
  delete :: Ptr a -> IO ()
  delete ptr = deleteInner ptr >> free' ptr
  {-# INLINE delete #-}

  -- | Free the content of the pointer recursively, but not the pointer itself.
  --
  -- For primitive types, this is equivalent to @const (pure ())@.
  deleteInner :: Ptr a -> IO ()

instance Pointable Int8 where
  type SizeOf Int8 = FromNat 1

  type IsDirect Int8 = 'True

  makeInner :: Ptr Int8 -> Int8 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Int8 -> IO Int8
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int8 -> Int8 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Int8 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Int16 where
  type SizeOf Int16 = FromNat 2

  type IsDirect Int16 = 'True

  makeInner :: Ptr Int16 -> Int16 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Int16 -> IO Int16
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int16 -> Int16 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Int16 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Int32 where
  type SizeOf Int32 = FromNat 4

  type IsDirect Int32 = 'True

  makeInner :: Ptr Int32 -> Int32 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Int32 -> IO Int32
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int32 -> Int32 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Int32 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Int64 where
  type SizeOf Int64 = FromNat PtrSize

  type IsDirect Int64 = 'True

  makeInner :: Ptr Int64 -> Int64 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Int64 -> IO Int64
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int64 -> Int64 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Int64 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Word8 where
  type SizeOf Word8 = FromNat 1

  type IsDirect Word8 = 'True

  makeInner :: Ptr Word8 -> Word8 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Word8 -> IO Word8
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word8 -> Word8 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Word8 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Word16 where
  type SizeOf Word16 = FromNat 2

  type IsDirect Word16 = 'True

  makeInner :: Ptr Word16 -> Word16 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Word16 -> IO Word16
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word16 -> Word16 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Word16 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Word32 where
  type SizeOf Word32 = FromNat 4

  type IsDirect Word32 = 'True

  makeInner :: Ptr Word32 -> Word32 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Word32 -> IO Word32
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word32 -> Word32 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Word32 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Word64 where
  type SizeOf Word64 = FromNat PtrSize

  type IsDirect Word64 = 'True

  makeInner :: Ptr Word64 -> Word64 -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Word64 -> IO Word64
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word64 -> Word64 -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Word64 -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

-- We want to know the size of "Int" at compile time, so we will always assume
-- it takes eight bytes.
instance Pointable Int where
  type SizeOf Int = FromNat PtrSize

  type IsDirect Int = 'True

  makeInner :: Ptr Int -> Int -> IO ()
  makeInner ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE makeInner #-}

  load :: Ptr Int -> IO Int
  load = fmap fromIntegral . peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr Int -> Int -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

  deleteInner :: Ptr Int -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

-- We want to know the size of "Word" at compile time, so we will always assume
-- it takes eight bytes.
instance Pointable Word where
  type SizeOf Word = FromNat PtrSize

  type IsDirect Word = 'True

  makeInner :: Ptr Word -> Word -> IO ()
  makeInner ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE makeInner #-}

  load :: Ptr Word -> IO Word
  load = fmap fromIntegral . peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr Word -> Word -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

  deleteInner :: Ptr Word -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

-- We want to know the size of "IntPtr" at compile time, so we will always
-- assume it takes eight bytes.
instance Pointable IntPtr where
  type SizeOf IntPtr = FromNat PtrSize

  type IsDirect IntPtr = 'True

  makeInner :: Ptr IntPtr -> IntPtr -> IO ()
  makeInner ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE makeInner #-}

  load :: Ptr IntPtr -> IO IntPtr
  load = fmap fromIntegral . peek @Int64 . castPtr
  {-# INLINE load #-}

  store :: Ptr IntPtr -> IntPtr -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

  deleteInner :: Ptr IntPtr -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

-- We want to know the size of "WordPtr" at compile time, so we will always
-- assume it takes eight bytes.
instance Pointable WordPtr where
  type SizeOf WordPtr = FromNat PtrSize

  type IsDirect WordPtr = 'True

  makeInner :: Ptr WordPtr -> WordPtr -> IO ()
  makeInner ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE makeInner #-}

  load :: Ptr WordPtr -> IO WordPtr
  load = fmap fromIntegral . peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr WordPtr -> WordPtr -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

  deleteInner :: Ptr WordPtr -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

-- We want to know the size of "Ptr" at compile time, so we will always assume
-- it takes eight bytes.
instance Pointable (Ptr a) where
  type SizeOf (Ptr a) = FromNat PtrSize

  type IsDirect (Ptr a) = 'True

  makeInner :: Ptr (Ptr a) -> Ptr a -> IO ()
  makeInner ptr = poke (castPtr ptr) . ptrToIntPtr
  {-# INLINE makeInner #-}

  load :: Ptr (Ptr a) -> IO (Ptr a)
  load = fmap intPtrToPtr . peek . castPtr
  {-# INLINE load #-}

  store :: Ptr (Ptr a) -> Ptr a -> IO ()
  store ptr = poke (castPtr ptr) . ptrToIntPtr
  {-# INLINE store #-}

  deleteInner :: Ptr (Ptr a) -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

-- We want to know the size of "Char" at compile time, so we will always assume
-- it takes four bytes.
instance Pointable Char where
  type SizeOf Char = FromNat 4

  type IsDirect Char = 'True

  makeInner :: Ptr Char -> Char -> IO ()
  makeInner ptr = poke (castPtr ptr) . fromIntegral @_ @Word32 . ord
  {-# INLINE makeInner #-}

  load :: Ptr Char -> IO Char
  load = fmap (chr . fromIntegral) . peek @Word32 . castPtr
  {-# INLINE load #-}

  store :: Ptr Char -> Char -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word32 . ord
  {-# INLINE store #-}

  deleteInner :: Ptr Char -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable () where
  type SizeOf () = FromNat 0

  type IsDirect () = 'True

  makeInner :: Ptr () -> () -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr () -> IO ()
  load = const $ pure ()
  {-# INLINE load #-}

  store :: Ptr () -> () -> IO ()
  store = const . const $ pure ()
  {-# INLINE store #-}

  deleteInner :: Ptr () -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Float where
  type SizeOf Float = FromNat 4

  type IsDirect Float = 'True

  makeInner :: Ptr Float -> Float -> IO ()
  makeInner ptr = poke (castPtr ptr)
  {-# INLINE makeInner #-}

  load :: Ptr Float -> IO Float
  load = peek
  {-# INLINE load #-}

  store :: Ptr Float -> Float -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Float -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Double where
  type SizeOf Double = FromNat PtrSize

  type IsDirect Double = 'True

  makeInner :: Ptr Double -> Double -> IO ()
  makeInner ptr = poke (castPtr ptr)
  {-# INLINE makeInner #-}

  load :: Ptr Double -> IO Double
  load = peek
  {-# INLINE load #-}

  store :: Ptr Double -> Double -> IO ()
  store = poke
  {-# INLINE store #-}

  deleteInner :: Ptr Double -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

instance Pointable Bool where
  type SizeOf Bool = FromNat 1

  type IsDirect Bool = 'True

  makeInner :: Ptr Bool -> Bool -> IO ()
  makeInner = store
  {-# INLINE makeInner #-}

  load :: Ptr Bool -> IO Bool
  load = fmap (/= 0) . peek @Word8 . castPtr
  {-# INLINE load #-}

  store :: Ptr Bool -> Bool -> IO ()
  store ptr b = poke (castPtr ptr) (if b then 1 else 0 :: Word8)
  {-# INLINE store #-}

  deleteInner :: Ptr Bool -> IO ()
  deleteInner _ = pure ()
  {-# INLINE deleteInner #-}

-- The difference between a tuple and a custom record is that a tuple is always
-- treated as a direct type, hence when a tuple is used as a field in a larger
-- data type, it is always allocated in the same chunk of memory as the parent
-- data type.
instance (Pointable a, Pointable b) => Pointable (a, b) where
  type SizeOf (a, b) = SizeOf (P2 a b)

  type IsDirect (a, b) = 'True

  makeInner :: Ptr (a, b) -> (a, b) -> IO ()
  makeInner ptr = store (castPtr ptr)
  {-# INLINE makeInner #-}

  load :: Ptr (a, b) -> IO (a, b)
  load ptr = do
    P2 a b <- load (castPtr ptr)
    pure (a, b)
  {-# INLINE load #-}

  store :: Ptr (a, b) -> (a, b) -> IO ()
  store ptr (a, b) = store (castPtr ptr) (P2 a b)
  {-# INLINE store #-}

  deleteInner :: Ptr (a, b) -> IO ()
  deleteInner ptr = deleteInner (castPtr ptr :: Ptr (P2 a b))
  {-# INLINE deleteInner #-}

instance (Pointable a, Pointable b, Pointable c) => Pointable (a, b, c) where
  type SizeOf (a, b, c) = SizeOf (P3 a b c)

  type IsDirect (a, b, c) = 'True

  makeInner :: Ptr (a, b, c) -> (a, b, c) -> IO ()
  makeInner ptr = store (castPtr ptr)
  {-# INLINE makeInner #-}

  load :: Ptr (a, b, c) -> IO (a, b, c)
  load ptr = do
    P3 a b c <- load (castPtr ptr)
    pure (a, b, c)
  {-# INLINE load #-}

  store :: Ptr (a, b, c) -> (a, b, c) -> IO ()
  store ptr (a, b, c) = store (castPtr ptr) (P3 a b c)
  {-# INLINE store #-}

  deleteInner :: Ptr (a, b, c) -> IO ()
  deleteInner ptr = deleteInner (castPtr ptr :: Ptr (P3 a b c))
  {-# INLINE deleteInner #-}

instance ( Pointable a
         , Pointable b
         , Pointable c
         , Pointable d )
  => Pointable (a, b, c, d) where
    type SizeOf (a, b, c, d) = SizeOf (P4 a b c d)

    type IsDirect (a, b, c, d) = 'True

    makeInner :: Ptr (a, b, c, d) -> (a, b, c, d) -> IO ()
    makeInner ptr = store (castPtr ptr)
    {-# INLINE makeInner #-}

    load :: Ptr (a, b, c, d) -> IO (a, b, c, d)
    load ptr = do
      P4 a b c d <- load (castPtr ptr)
      pure (a, b, c, d)
    {-# INLINE load #-}

    store :: Ptr (a, b, c, d) -> (a, b, c, d) -> IO ()
    store ptr (a, b, c, d) = store (castPtr ptr) (P4 a b c d)
    {-# INLINE store #-}

    deleteInner :: Ptr (a, b, c, d) -> IO ()
    deleteInner ptr = deleteInner (castPtr ptr :: Ptr (P4 a b c d))
    {-# INLINE deleteInner #-}

instance ( Pointable a
         , Pointable b
         , Pointable c
         , Pointable d
         , Pointable e )
  => Pointable (a, b, c, d, e) where
    type SizeOf (a, b, c, d, e) = SizeOf (P5 a b c d e)

    type IsDirect (a, b, c, d, e) = 'True

    makeInner :: Ptr (a, b, c, d, e) -> (a, b, c, d, e) -> IO ()
    makeInner ptr = store (castPtr ptr)
    {-# INLINE makeInner #-}

    load :: Ptr (a, b, c, d, e) -> IO (a, b, c, d, e)
    load ptr = do
      P5 a b c d e <- load (castPtr ptr)
      pure (a, b, c, d, e)
    {-# INLINE load #-}

    store :: Ptr (a, b, c, d, e) -> (a, b, c, d, e) -> IO ()
    store ptr (a, b, c, d, e) = store (castPtr ptr) (P5 a b c d e)
    {-# INLINE store #-}

    deleteInner :: Ptr (a, b, c, d, e) -> IO ()
    deleteInner ptr = deleteInner (castPtr ptr :: Ptr (P5 a b c d e))
    {-# INLINE deleteInner #-}


--------------------------------------------------------------------------------
-- Generic Derivation Wrapper
--------------------------------------------------------------------------------

-- | A wrapper used for deriving @Pointable@ instances for @Generic@ types.
newtype WithPointable a = WithPointable { unWithPointable :: a }

-- The instance is derived from @GPointable@, a generic version of @Pointable@.
instance ( Generic a
         , GPointable (Rep a)
         , KnownBool (GIsPrim (Rep a)) )
  => Pointable (WithPointable a) where
    type SizeOf (WithPointable a) = GSizeOf (Rep a)

    type IsDirect (WithPointable a) = GIsPrim (Rep a)

    makeInner :: Ptr (WithPointable a) -> WithPointable a -> IO ()
    makeInner ptr a = gMake (castPtr ptr) . from $ unWithPointable a
    {-# INLINE makeInner #-}

    load :: Ptr (WithPointable a) -> IO (WithPointable a)
    load ptr = do
      let ptr' = castPtr ptr :: Ptr (Rep a p)
      WithPointable . to <$> gLoad ptr'
    {-# INLINE load #-}

    store :: Ptr (WithPointable a) -> WithPointable a -> IO ()
    store ptr a = do
      let ptr' = castPtr ptr :: Ptr (Rep a p)
      gStore ptr' $ from $ unWithPointable a
    {-# INLINE store #-}

    deleteInner :: Ptr (WithPointable a) -> IO ()
    deleteInner ptr = gDelete (castPtr ptr :: Ptr (Rep a p))
    {-# INLINE deleteInner #-}


--------------------------------------------------------------------------------
-- Generic Pointable
--------------------------------------------------------------------------------

class (Val (GSizeOf a)) => GPointable a where
  -- See @MyNat@ for more information.
  type GSizeOf a :: MyNat

  type GIsPrim a :: Bool

  gSize :: a p -> Int
  gSize = const . fromIntegral $ val (Proxy @(GSizeOf a))
  {-# INLINE gSize #-}

  -- | The top-layer pointer is already provided.
  gMake :: Ptr (a p) -> a p -> IO ()

  gLoad :: Ptr (a p) -> IO (a p)

  gStore :: Ptr (a p) -> a p -> IO ()

  -- | The top-layer pointer is not freed here, so in particular for direct
  -- types this function does nothing.
  gDelete :: Ptr (a p) -> IO ()

instance GPointable U1 where
  type GSizeOf U1 = FromNat 0

  type GIsPrim U1 = 'True

  gMake :: Ptr (U1 p) -> U1 p -> IO ()
  gMake _ _ = pure ()
  {-# INLINE gMake #-}

  gLoad :: Ptr (U1 p) -> IO (U1 p)
  gLoad = const $ pure U1
  {-# INLINE gLoad #-}

  gStore :: Ptr (U1 p) -> U1 p -> IO ()
  gStore = const . const $ pure ()
  {-# INLINE gStore #-}

  gDelete :: Ptr (U1 p) -> IO ()
  gDelete = const $ pure ()
  {-# INLINE gDelete #-}

instance GPointable a => GPointable (M1 D c a) where
  type GSizeOf (M1 D c a) = GSizeOf a

  type GIsPrim (M1 D c a) = 'False -- 'False for custom data types

  gMake :: Ptr (M1 D c a p) -> M1 i c a p -> IO ()
  gMake ptr a = gMake (castPtr ptr) (unM1 a)
  {-# INLINE gMake #-}

  gLoad :: Ptr (M1 D c a p) -> IO (M1 D c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (M1 D c a p) -> M1 D c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

  gDelete :: Ptr (M1 D c a p) -> IO ()
  gDelete ptr = gDelete (castPtr ptr :: Ptr (a p))
  {-# INLINE gDelete #-}

instance GPointable a => GPointable (M1 C c a) where
  type GSizeOf (M1 C c a) = GSizeOf a

  type GIsPrim (M1 C c a) = GIsPrim a

  gMake :: Ptr (M1 C c a p) -> M1 i c a p -> IO ()
  gMake ptr a = gMake (castPtr ptr) (unM1 a)
  {-# INLINE gMake #-}

  gLoad :: Ptr (M1 C c a p) -> IO (M1 C c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (M1 C c a p) -> M1 C c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

  gDelete :: Ptr (M1 C c a p) -> IO ()
  gDelete ptr = gDelete (castPtr ptr :: Ptr (a p))
  {-# INLINE gDelete #-}

instance GPointable a => GPointable (M1 S c a) where
  type GSizeOf (M1 S c a) = GSizeOf a

  type GIsPrim (M1 S c a) = GIsPrim a

  gMake :: Ptr (M1 S c a p) -> M1 i c a p -> IO ()
  gMake ptr a = gMake (castPtr ptr) (unM1 a)
  {-# INLINE gMake #-}

  gLoad :: Ptr (M1 S c a p) -> IO (M1 S c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (M1 S c a p) -> M1 S c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

  gDelete :: Ptr (M1 S c a p) -> IO ()
  gDelete ptr = gDelete (castPtr ptr :: Ptr (a p))
  {-# INLINE gDelete #-}

-- The implementation for direct and indirect types are evidently different.
-- For example, in terms of @gMake@, we can just store the value directly into
-- the pointer for a direct type, but for indirect we need to allocate a new
-- reference pointer before allocating @gSize@ bytes and store the content into
-- the reference pointer.
--
-- We use @IsDirect@ to distinguish the two cases and dispatch the appropriate
-- implementation in @K1Helper@.
instance ( Val (GSizeOf (K1 i a))
         , Pointable a
         , IsDirect a ~ f )
  => GPointable (K1 i a) where
    type GSizeOf (K1 i a) = 'Si (IsDirect a) ('TypeNat a) (FromNat PtrSize)

    type GIsPrim (K1 i a) = IsDirect a

    gMake :: Ptr (K1 i a p) -> K1 i a p -> IO ()
    gMake ptr a = if boolVal (Proxy @f)
      then store (castPtr ptr) $ unK1 a
      else do
        ptr' <- make (unK1 a)
        poke (castPtr ptr) ptr'
    {-# INLINE gMake #-}

    gLoad :: Ptr (K1 i a p) -> IO (K1 i a p)
    gLoad ptr = if boolVal (Proxy @f)
      then fmap K1 . load $ castPtr ptr
      else do
        ptr' <- peek (castPtr ptr :: Ptr (Ptr a))
        K1 <$> load ptr'
    {-# INLINE gLoad #-}

    gStore :: Ptr (K1 i a p) -> K1 i a p -> IO ()
    gStore ptr a = if boolVal (Proxy @f)
      then store (castPtr ptr) (unK1 a)
      else do
        ptr' <- peek (castPtr ptr :: Ptr (Ptr a))
        store ptr' (unK1 a)
    {-# INLINE gStore #-}

    gDelete :: Ptr (K1 i a p) -> IO ()
    gDelete ptr = if boolVal (Proxy @f)
      then pure ()
      else peek (castPtr ptr :: Ptr (Ptr a)) >>= delete
    {-# INLINE gDelete #-}

-- Store and load for direct and indirect fields are different, hence the
-- helper type class @HasSLSize@ is introduced to dispatch the appropriate
-- implementation.
instance ( Val (GSizeOf (a :*: b))
         , GPointable a
         , GPointable b
         , Val (SLSize f a)
         , GIsPrim a ~ f
         , GIsPrim b ~ g
         , KnownBool f
         , KnownBool g )
  => GPointable (a :*: b) where
    type GSizeOf (a :*: b) = Sum (GSizeOf a) (GSizeOf b)

    -- The product type itself is direct regardless of the fields. The
    -- "indirection" will come from the data meta @M1 D@.
    type GIsPrim (a :*: b) = 'True

    gMake :: Ptr ((a :*: b) p) -> (a :*: b) p -> IO ()
    gMake ptr x@(a :*: b) = do
      gMake (castPtr ptr) a
      gMake (castPtr ptr `plusPtr` slSize (Proxy @f) (Proxy @a)) b
    {-# INLINE gMake #-}

    gLoad :: Ptr ((a :*: b) p) -> IO ((a :*: b) p)
    gLoad ptr = do
      a <- gLoad $ castPtr ptr
      b <- gLoad $ castPtr ptr `plusPtr` slSize (Proxy @f) (Proxy @a)
      pure $ a :*: b
    {-# INLINE gLoad #-}

    gStore :: Ptr ((a :*: b) p) -> (a :*: b) p -> IO ()
    gStore ptr (a :*: b) = do
      gStore (castPtr ptr) a
      gStore (castPtr ptr `plusPtr` slSize (Proxy @f) (Proxy @a)) b
    {-# INLINE gStore #-}

    gDelete :: Ptr ((a :*: b) p) -> IO ()
    gDelete ptr = do
      gDelete (castPtr ptr :: Ptr (a p))
      gDelete (castPtr ptr `plusPtr` slSize (Proxy @f) (Proxy @a) :: Ptr (b p))
    {-# INLINE gDelete #-}

-- | Determines the size for store and load. For indirect fields, it should
-- be always the pointer size, i.e. 8 bytes. For direct fields, it should be
-- the size of the field.
class (Val (SLSize f a), GPointable a)
  => HasSLSize (f :: Bool) (a :: Type -> Type) where
    type SLSize f a :: MyNat

    slSize :: Proxy f -> Proxy a -> Int
    slSize _ _ = fromIntegral $ val (Proxy @(SLSize f a))

instance (Val (GSizeOf a), KnownBool f, GPointable a) => HasSLSize f a where
  type SLSize f a = Si f (GSizeOf a) (FromNat PtrSize)


--------------------------------------------------------------------------------
-- Helper Types & Functions
--------------------------------------------------------------------------------

-- | A type-level natural number that refer to the size of a type.
--
-- It is used to break the recursive knot in the definition of 'GSizeOf'.
--
-- For example, suppose we have the following type:
--
-- > data XRec = XRec XRec
--
-- Although this data type is pretty meaningless because it is infinitely
-- recursive, it's "size" should be 8 (the pointer size) since the inner @XRec@
-- should be represented by a pointer.
-- If we define @GSizeOf@ using @Nat@, then during compilation, GHC will try to
-- evaluate @GSizeOf XRec@, which is @If (GIsPrim XRec) (SizeOf XRec) 8@.
-- However, despite @GIsPrim XRec@ is @'False@, GHC would still attempt to
-- expand both branches of the @If@, leading to an infinite loop.
--
-- If we wrap it using @MyNat@, then @GSizeOf XRec@ will be
-- @'Si (GIsPrim XRec) XRec 8@, which then simplifies to @8@ since @XRec@ cannot
-- be further evaluated.
data MyNat = Zero
           | Succ MyNat
           | TypeNat Type
           | Si Bool MyNat MyNat
           | Sum MyNat MyNat

type family FromNat (n :: Nat) :: MyNat where
  FromNat 0 = 'Zero
  FromNat n = 'Succ (FromNat (n - 1))

type family ToNat (n :: MyNat) :: Nat where
  ToNat 'Zero            = 0
  ToNat ('Succ n)        = 1 + ToNat n
  ToNat ('TypeNat t)     = ToNat (SizeOf t)
  ToNat ('Si 'True a b)  = ToNat a
  ToNat ('Si 'False a b) = ToNat b
  ToNat ('Sum a b)       = ToNat a + ToNat b

class KnownBool (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance KnownBool 'True where
  boolVal :: Proxy 'True -> Bool
  boolVal _ = True
  {-# INLINE boolVal #-}

instance KnownBool 'False where
  boolVal :: Proxy 'False -> Bool
  boolVal _ = False
  {-# INLINE boolVal #-}

-- | A class with a method that computes the term-level size of a @MyNat@.
--
-- It basically does the job of @natVal@, but for @MyNat@. It is used here
-- because the derivation of @KnownNat@ is very limited by GHC.
class Val (n :: MyNat) where
  val :: Proxy n -> Int

instance Val 'Zero where
  val :: Proxy 'Zero -> Int
  val _ = 0
  {-# INLINE val #-}

instance Val n => Val ('Succ n) where
  val :: Proxy ('Succ n) -> Int
  val _ = 1 + val (Proxy @n)
  {-# INLINE val #-}

instance Val (SizeOf n) => Val ('TypeNat n) where
  val :: Proxy ('TypeNat n) -> Int
  val _ = fromIntegral $ val (Proxy @(SizeOf n))
  {-# INLINE val #-}

instance (Val a, Val b, KnownBool f) => Val ('Si f a b) where
  val :: Proxy ('Si f a b) -> Int
  val _ = if boolVal (Proxy @f) then val (Proxy @a) else val (Proxy @b)
  {-# INLINE val #-}

instance (Val a, Val b) => Val ('Sum a b) where
  val :: Proxy ('Sum a b) -> Int
  val _ = val (Proxy @a) + val (Proxy @b)
  {-# INLINE val #-}

-- | Helper type for the @Pointable@ derivation for tuples.
data P2 a b = P2 a b
  deriving (Generic)
  deriving (Pointable) via (WithPointable (P2 a b))

-- | Helper type for the @Pointable@ derivation for triples.
data P3 a b c = P3 a b c
  deriving (Generic)
  deriving (Pointable) via (WithPointable (P3 a b c))

-- | Helper type for the @Pointable@ derivation for quadruples.
data P4 a b c d = P4 a b c d
  deriving (Generic)
  deriving (Pointable) via (WithPointable (P4 a b c d))

-- | Helper type for the @Pointable@ derivation for quintuples.
data P5 a b c d e = P5 a b c d e
  deriving (Generic)
  deriving (Pointable) via (WithPointable (P5 a b c d e))

type PtrSize = 8

ptrSize :: Int
ptrSize = val (Proxy @(FromNat PtrSize))
{-# INLINE ptrSize #-}

-- | For debugging purpose.
#ifdef CEA_DEBUG
free' :: forall a. (Pointable a, Val (SizeOf a)) => Ptr a -> IO ()
free' ptr = do
  putStrLn $ "Freeing " ++ show (val (Proxy @(SizeOf a))) ++ " byte(s)."
  free ptr
{-# INLINE free' #-}
#else
free' :: forall a. (Pointable a, Val (SizeOf a)) => Ptr a -> IO ()
free' = free
{-# INLINE free' #-}
#endif
