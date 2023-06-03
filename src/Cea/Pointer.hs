-- | This module contains the type class @Pointable@, which enables the generic
-- marshalling between C-pointer and Haskell data types. It implements
-- @Pointable@ for the "primitive" types, and provides a generic implementation
-- for most other types (TODO: Sum type not supported yet).
--
-- The only exposed functions are the methods of the @Pointable@ type class.
-- Others are implementation details that can be changed without prior notice.
module Cea.Pointer where

import           Data.Char
import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Word
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
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
class (Val (SizeOf a), KnownBool (IsPrim a)) => Pointable a where
  -- | The size of the type in bytes.
  type SizeOf a :: MyNat

  -- | Whether the type is a primitive type. If the type is not a primitive,
  -- it will be stored via a layer of pointer indirection.
  --
  -- By default, it is @'True@ for all concrete instances in this module, and
  -- @'False@ for all user-defined types.
  type IsPrim a :: Bool

  -- | Term-level version of @SizeOf@.
  size :: a -> Int
  size _ = fromIntegral $ val (Proxy :: Proxy (SizeOf a))

  -- | Create a new pointer and store the value in it.
  make :: a -> IO (Ptr a)

  -- | Load the value from the pointer.
  load :: Ptr a -> IO a

  -- | Store the value in the pointer.
  store :: Ptr a -> a -> IO ()

instance Pointable Int8 where
  type SizeOf Int8 = FromNat 1

  type IsPrim Int8 = 'True

  make :: Int8 -> IO (Ptr Int8)
  make = new
  {-# INLINE make #-}

  load :: Ptr Int8 -> IO Int8
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int8 -> Int8 -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Int16 where
  type SizeOf Int16 = FromNat 2

  type IsPrim Int16 = 'True

  make :: Int16 -> IO (Ptr Int16)
  make = new
  {-# INLINE make #-}

  load :: Ptr Int16 -> IO Int16
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int16 -> Int16 -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Int32 where
  type SizeOf Int32 = FromNat 4

  type IsPrim Int32 = 'True

  make :: Int32 -> IO (Ptr Int32)
  make = new
  {-# INLINE make #-}

  load :: Ptr Int32 -> IO Int32
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int32 -> Int32 -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Int64 where
  type SizeOf Int64 = FromNat PtrSize

  type IsPrim Int64 = 'True

  make :: Int64 -> IO (Ptr Int64)
  make = new
  {-# INLINE make #-}

  load :: Ptr Int64 -> IO Int64
  load = peek
  {-# INLINE load #-}

  store :: Ptr Int64 -> Int64 -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Word8 where
  type SizeOf Word8 = FromNat 1

  type IsPrim Word8 = 'True

  make :: Word8 -> IO (Ptr Word8)
  make = new
  {-# INLINE make #-}

  load :: Ptr Word8 -> IO Word8
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word8 -> Word8 -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Word16 where
  type SizeOf Word16 = FromNat 2

  type IsPrim Word16 = 'True

  make :: Word16 -> IO (Ptr Word16)
  make = new
  {-# INLINE make #-}

  load :: Ptr Word16 -> IO Word16
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word16 -> Word16 -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Word32 where
  type SizeOf Word32 = FromNat 4

  type IsPrim Word32 = 'True

  make :: Word32 -> IO (Ptr Word32)
  make = new
  {-# INLINE make #-}

  load :: Ptr Word32 -> IO Word32
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word32 -> Word32 -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Word64 where
  type SizeOf Word64 = FromNat PtrSize

  type IsPrim Word64 = 'True

  make :: Word64 -> IO (Ptr Word64)
  make = new
  {-# INLINE make #-}

  load :: Ptr Word64 -> IO Word64
  load = peek
  {-# INLINE load #-}

  store :: Ptr Word64 -> Word64 -> IO ()
  store = poke
  {-# INLINE store #-}

-- We want to know the size of "Int" at compile time, so we will always assume
-- it takes eight bytes.
instance Pointable Int where
  type SizeOf Int = FromNat PtrSize

  type IsPrim Int = 'True

  make :: Int -> IO (Ptr Int)
  make = fmap castPtr . new @Word64 . fromIntegral
  {-# INLINE make #-}

  load :: Ptr Int -> IO Int
  load = fmap fromIntegral . peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr Int -> Int -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

-- We want to know the size of "Word" at compile time, so we will always assume
-- it takes eight bytes.
instance Pointable Word where
  type SizeOf Word = FromNat PtrSize

  type IsPrim Word = 'True

  make :: Word -> IO (Ptr Word)
  make = fmap castPtr . new @Word64 . fromIntegral
  {-# INLINE make #-}

  load :: Ptr Word -> IO Word
  load = fmap fromIntegral . peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr Word -> Word -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

-- We want to know the size of "IntPtr" at compile time, so we will always
-- assume it takes eight bytes.
instance Pointable IntPtr where
  type SizeOf IntPtr = FromNat PtrSize

  type IsPrim IntPtr = 'True

  make :: IntPtr -> IO (Ptr IntPtr)
  make = fmap castPtr . new @Int64 . fromIntegral
  {-# INLINE make #-}

  load :: Ptr IntPtr -> IO IntPtr
  load = fmap fromIntegral . peek @Int64 . castPtr
  {-# INLINE load #-}

  store :: Ptr IntPtr -> IntPtr -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

-- We want to know the size of "Ptr" at compile time, so we will always assume
-- it takes eight bytes.

instance Pointable (Ptr a) where
  type SizeOf (Ptr a) = FromNat PtrSize

  type IsPrim (Ptr a) = 'True

  make :: Ptr a -> IO (Ptr (Ptr a))
  make = fmap castPtr . new . ptrToIntPtr
  {-# INLINE make #-}

  load :: Ptr (Ptr a) -> IO (Ptr a)
  load = fmap intPtrToPtr . peek . castPtr
  {-# INLINE load #-}

  store :: Ptr (Ptr a) -> Ptr a -> IO ()
  store ptr = poke (castPtr ptr) . ptrToIntPtr
  {-# INLINE store #-}

-- We want to know the size of "Char" at compile time, so we will always assume
-- it takes four bytes.
instance Pointable Char where
  type SizeOf Char = FromNat 4

  type IsPrim Char = 'True

  make :: Char -> IO (Ptr Char)
  make = fmap castPtr . new @Word32 . fromIntegral . ord
  {-# INLINE make #-}

  load :: Ptr Char -> IO Char
  load = fmap (chr . fromIntegral) . peek @Word32 . castPtr
  {-# INLINE load #-}

  store :: Ptr Char -> Char -> IO ()
  store ptr = poke (castPtr ptr) . fromIntegral @_ @Word32 . ord
  {-# INLINE store #-}

instance Pointable () where
  type SizeOf () = FromNat 0

  type IsPrim () = 'True

  make :: () -> IO (Ptr ())
  make = const . pure $ nullPtr
  {-# INLINE make #-}

  load :: Ptr () -> IO ()
  load = const $ pure ()
  {-# INLINE load #-}

  store :: Ptr () -> () -> IO ()
  store = const . const $ pure ()
  {-# INLINE store #-}

instance Pointable Float where
  type SizeOf Float = FromNat 4

  type IsPrim Float = 'True

  make :: Float -> IO (Ptr Float)
  make = new
  {-# INLINE make #-}

  load :: Ptr Float -> IO Float
  load = peek
  {-# INLINE load #-}

  store :: Ptr Float -> Float -> IO ()
  store = poke
  {-# INLINE store #-}

instance Pointable Double where
  type SizeOf Double = FromNat PtrSize

  type IsPrim Double = 'True

  make :: Double -> IO (Ptr Double)
  make = new
  {-# INLINE make #-}

  load :: Ptr Double -> IO Double
  load = peek
  {-# INLINE load #-}

  store :: Ptr Double -> Double -> IO ()
  store = poke
  {-# INLINE store #-}


-- The difference between a tuple and a custom record is that a tuple is always
-- treated as a primitive type, hence when a tuple is used as a field in a
-- larger data type, it is always allocated in the same chunk of memory as the
-- parent data type.
instance (Pointable a, Pointable b) => Pointable (a, b) where
  type SizeOf (a, b) = SizeOf (P2 a b)

  type IsPrim (a, b) = 'True

  make :: (a, b) -> IO (Ptr (a, b))
  make (a, b) = castPtr <$> make (P2 a b)
  {-# INLINE make #-}

  load :: Ptr (a, b) -> IO (a, b)
  load ptr = do
    P2 a b <- load (castPtr ptr)
    pure (a, b)
  {-# INLINE load #-}

  store :: Ptr (a, b) -> (a, b) -> IO ()
  store ptr (a, b) = store (castPtr ptr) (P2 a b)
  {-# INLINE store #-}

instance (Pointable a, Pointable b, Pointable c) => Pointable (a, b, c) where
  type SizeOf (a, b, c) = SizeOf (P3 a b c)

  type IsPrim (a, b, c) = 'True

  make :: (a, b, c) -> IO (Ptr (a, b, c))
  make (a, b, c) = castPtr <$> make (P3 a b c)
  {-# INLINE make #-}

  load :: Ptr (a, b, c) -> IO (a, b, c)
  load ptr = do
    P3 a b c <- load (castPtr ptr)
    pure (a, b, c)
  {-# INLINE load #-}

  store :: Ptr (a, b, c) -> (a, b, c) -> IO ()
  store ptr (a, b, c) = store (castPtr ptr) (P3 a b c)
  {-# INLINE store #-}

instance ( Pointable a
         , Pointable b
         , Pointable c
         , Pointable d )
  => Pointable (a, b, c, d) where
    type SizeOf (a, b, c, d) = SizeOf (P4 a b c d)

    type IsPrim (a, b, c, d) = 'True

    make :: (a, b, c, d) -> IO (Ptr (a, b, c, d))
    make (a, b, c, d) = castPtr <$> make (P4 a b c d)
    {-# INLINE make #-}

    load :: Ptr (a, b, c, d) -> IO (a, b, c, d)
    load ptr = do
      P4 a b c d <- load (castPtr ptr)
      pure (a, b, c, d)
    {-# INLINE load #-}

    store :: Ptr (a, b, c, d) -> (a, b, c, d) -> IO ()
    store ptr (a, b, c, d) = store (castPtr ptr) (P4 a b c d)
    {-# INLINE store #-}

instance ( Pointable a
         , Pointable b
         , Pointable c
         , Pointable d
         , Pointable e )
  => Pointable (a, b, c, d, e) where
    type SizeOf (a, b, c, d, e) = SizeOf (P5 a b c d e)

    type IsPrim (a, b, c, d, e) = 'True

    make :: (a, b, c, d, e) -> IO (Ptr (a, b, c, d, e))
    make (a, b, c, d, e) = castPtr <$> make (P5 a b c d e)
    {-# INLINE make #-}

    load :: Ptr (a, b, c, d, e) -> IO (a, b, c, d, e)
    load ptr = do
      P5 a b c d e <- load (castPtr ptr)
      pure (a, b, c, d, e)
    {-# INLINE load #-}

    store :: Ptr (a, b, c, d, e) -> (a, b, c, d, e) -> IO ()
    store ptr (a, b, c, d, e) = store (castPtr ptr) (P5 a b c d e)
    {-# INLINE store #-}


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
    -- Note that @GSizeOf@ does not build @Nat@ kinds, and we convert it via
    -- @ToNat@. This is because GHC's evaluation strategy may not converge for
    -- self-referencing types.
    type SizeOf (WithPointable a) = GSizeOf (Rep a)

    type IsPrim (WithPointable a) = GIsPrim (Rep a)

    make :: WithPointable a -> IO (Ptr (WithPointable a))
    make a = do
      ptr <- gMake . from $ unWithPointable a
      pure $ castPtr ptr
    {-# INLINE make #-}

    load :: Ptr (WithPointable a) -> IO (WithPointable a)
    load ptr = do
      let ptr' = castPtr ptr :: Ptr (Rep a p)
      d <- gLoad ptr'
      pure $ WithPointable $ to d
    {-# INLINE load #-}

    store :: Ptr (WithPointable a) -> WithPointable a -> IO ()
    store ptr a = do
      let ptr' = castPtr ptr :: Ptr (Rep a p)
      gStore ptr' $ from $ unWithPointable a
    {-# INLINE store #-}


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

  gMake :: a p -> IO (Ptr (a p))

  gLoad :: Ptr (a p) -> IO (a p)

  gStore :: Ptr (a p) -> a p -> IO ()

instance GPointable U1 where
  type GSizeOf U1 = FromNat 0

  type GIsPrim U1 = 'True

  gMake :: U1 p -> IO (Ptr (U1 p))
  gMake = const . pure $ nullPtr
  {-# INLINE gMake #-}

  gLoad :: Ptr (U1 p) -> IO (U1 p)
  gLoad = const $ pure U1
  {-# INLINE gLoad #-}

  gStore :: Ptr (U1 p) -> U1 p -> IO ()
  gStore = const . const $ pure ()
  {-# INLINE gStore #-}

instance GPointable a => GPointable (M1 D c a) where
  type GSizeOf (M1 D c a) = GSizeOf a

  type GIsPrim (M1 D c a) = 'False -- 'False for custom data types

  gMake :: M1 i c a p -> IO (Ptr (M1 D c a p))
  gMake = fmap castPtr . gMake . unM1
  {-# INLINE gMake #-}

  gLoad :: Ptr (M1 D c a p) -> IO (M1 D c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (M1 D c a p) -> M1 D c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

instance GPointable a => GPointable (M1 C c a) where
  type GSizeOf (M1 C c a) = GSizeOf a

  type GIsPrim (M1 C c a) = GIsPrim a

  gMake :: M1 i c a p -> IO (Ptr (M1 C c a p))
  gMake = fmap castPtr . gMake . unM1
  {-# INLINE gMake #-}

  gLoad :: Ptr (M1 C c a p) -> IO (M1 C c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (M1 C c a p) -> M1 C c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

instance GPointable a => GPointable (M1 S c a) where
  type GSizeOf (M1 S c a) = GSizeOf a

  type GIsPrim (M1 S c a) = GIsPrim a

  gMake :: M1 i c a p -> IO (Ptr (M1 S c a p))
  gMake = fmap castPtr . gMake . unM1
  {-# INLINE gMake #-}

  gLoad :: Ptr (M1 S c a p) -> IO (M1 S c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (M1 S c a p) -> M1 S c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

-- The implementation for primitive and non-primitive types are evidently
-- different. For example, in terms of @gMake@, we can just allocate
-- @gSize@ bytes for a primitive type, but for non-primitives we need to
-- allocate 8 bytes for the pointer before allocating @gSize@ bytes and store
-- it into the reference pointer.
--
-- We use @IsPrim@ to distinguish the two cases and dispatch the appropriate
-- implementation in @K1Helper@.
instance ( Val (GSizeOf (K1 i a))
         , Pointable a
         , IsPrim a ~ f )
  => GPointable (K1 i a) where
    type GSizeOf (K1 i a) = 'Si (IsPrim a) ('TypeNat a) (FromNat PtrSize)

    type GIsPrim (K1 i a) = IsPrim a

    gMake :: K1 i a p -> IO (Ptr (K1 i a p))
    gMake a = if boolVal (Proxy @f)
      then fmap castPtr . make $ unK1 a
      else do 
        ptr  <- make (unK1 a)
        ptr' <- mallocBytes ptrSize
        poke ptr' ptr
        pure $ castPtr ptr'
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

-- Store and load for primitive and non-primitive fields are different, hence
-- the helper type class @MkSpace@ is introduced to dispatch the appropriate
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

    -- The product type itself is primitive regardless of the fields. The
    -- "indirection" will come from the data meta @M1 D@.
    type GIsPrim (a :*: b) = 'True

    gMake :: (a :*: b) p -> IO (Ptr ((a :*: b) p))
    gMake a = do
      ptr <- mallocBytes $ gSize a
      mkSpace (castPtr ptr) (Proxy @f) (Proxy @a)
      mkSpace (castPtr ptr `plusPtr` ptrSize) (Proxy @g) (Proxy @b)
      gStore ptr a
      pure ptr
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

-- | Determines the size for store and load. For non-primitive fields, it should
-- be always the pointer size, i.e. 8 bytes. For primitive fields, it should be
-- the size of the field.
class Val (SLSize f a) => MkSpace (f :: Bool) (a :: * -> *) where
  type SLSize f a :: MyNat

  slSize :: Proxy f -> Proxy a -> Int
  slSize _ _ = fromIntegral $ val (Proxy @(SLSize f a))

  mkSpace :: Ptr (a p) -> Proxy f -> Proxy a -> IO ()

instance (Val (GSizeOf a), KnownBool f) => MkSpace f a where
  type SLSize f a = Si f (GSizeOf a) (FromNat PtrSize)

  mkSpace :: Ptr (a p) -> Proxy f -> Proxy a -> IO ()
  mkSpace ptr _ _ = if boolVal (Proxy @f)
    then pure ()
    else do
      ptr' <- mallocBytes . fromIntegral $ val (Proxy @(GSizeOf a))
      poke (castPtr ptr :: Ptr (Ptr Int64)) (castPtr ptr' :: Ptr Int64)
  {-# INLINE mkSpace #-}


--------------------------------------------------------------------------------
-- Helper Types
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
  ToNat 'Zero = 0
  ToNat ('Succ n) = 1 + ToNat n
  ToNat ('TypeNat t) = ToNat (SizeOf t)
  ToNat ('Si 'True a b) = ToNat a
  ToNat ('Si 'False a b) = ToNat b
  ToNat ('Sum a b) = ToNat a + ToNat b

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
