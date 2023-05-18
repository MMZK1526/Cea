module Cea.Pointer where

import           Data.Char
import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Type.Bool
import           Data.Word
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           GHC.Generics
import           GHC.TypeLits

class Pointable a where
  type SizeOf a :: Nat

  type IsPrim a :: Bool

  sizeOf :: KnownNat (SizeOf a) => a -> Int
  sizeOf _ = fromIntegral $ natVal (Proxy :: Proxy (SizeOf a))

  make :: KnownNat (SizeOf a) => a -> IO (Ptr a)

  load :: KnownNat (SizeOf a) => Ptr a -> IO a

  store :: KnownNat (SizeOf a) => Ptr a -> a -> IO ()

instance Pointable Int8 where
  type SizeOf Int8 = 1

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
  type SizeOf Int16 = 2

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
  type SizeOf Int32 = 4

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
  type SizeOf Int64 = 8

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
  type SizeOf Word8 = 1

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
  type SizeOf Word16 = 2

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
  type SizeOf Word32 = 4

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
  type SizeOf Word64 = 8

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
  type SizeOf Int = 8

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
  type SizeOf Word = 8

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

-- We want to know the size of "Char" at compile time, so we will always assume
-- it takes four bytes.
instance Pointable Char where
  type SizeOf Char = 4

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
  type SizeOf () = 0

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
  type SizeOf Float = 4

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
  type SizeOf Double = 8

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


--------------------------------------------------------------------------------
-- Generic Derivation Wrapper
--------------------------------------------------------------------------------

newtype WithPointable a = WithPointable { unWithPointable :: a }

instance (Generic a, GPointable (Rep a)) => Pointable (WithPointable a) where
  type SizeOf (WithPointable a) = ToNat (GSizeOf (Rep a))

  type IsPrim (WithPointable a) = GIsPrim (Rep a)

  make :: KnownNat (SizeOf (WithPointable a)) => WithPointable a -> IO (Ptr (WithPointable a))
  make a = do
    ptr <- gMake . from $ unWithPointable a
    pure $ castPtr ptr
  {-# INLINE make #-}

  load :: KnownNat (SizeOf (WithPointable a)) => Ptr (WithPointable a) -> IO (WithPointable a)
  load ptr = do
    let ptr' = castPtr ptr :: Ptr (Rep a p)
    d <- gLoad ptr'
    pure $ WithPointable $ to d
  {-# INLINE load #-}

  store :: KnownNat (SizeOf (WithPointable a)) => Ptr (WithPointable a) -> WithPointable a -> IO ()
  store ptr a = do
    let ptr' = castPtr ptr :: Ptr (Rep a p)
    gStore ptr' $ from $ unWithPointable a
  {-# INLINE store #-}


--------------------------------------------------------------------------------
-- Generic Pointable
--------------------------------------------------------------------------------

class GPointable a where
  type GSizeOf a :: MyNat

  type GIsPrim a :: Bool

  gSizeOf :: KnownNat (ToNat (GSizeOf a)) => a p -> Int
  gSizeOf = const . fromIntegral $ natVal (Proxy @(ToNat (GSizeOf a)))
  {-# INLINE gSizeOf #-}

  gMake :: KnownNat (ToNat (GSizeOf a)) => a p -> IO (Ptr (a p))

  gLoad :: KnownNat (ToNat (GSizeOf a)) => Ptr (a p) -> IO (a p)

  gStore :: KnownNat (ToNat (GSizeOf a)) => Ptr (a p) -> a p -> IO ()

instance GPointable U1 where
  type GSizeOf U1 = 'MyNat 0

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

instance GPointable a => GPointable (M1 i c a) where
  type GSizeOf (M1 i c a) = GSizeOf a

  type GIsPrim (M1 i c a) = GIsPrim a

  gMake :: KnownNat (ToNat (GSizeOf (M1 i c a))) => M1 i c a p -> IO (Ptr (M1 i c a p))
  gMake = fmap castPtr . gMake . unM1
  {-# INLINE gMake #-}

  gLoad :: KnownNat (ToNat (GSizeOf (M1 i c a))) => Ptr (M1 i c a p) -> IO (M1 i c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: KnownNat (ToNat (GSizeOf (M1 i c a))) => Ptr (M1 i c a p) -> M1 i c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

instance (Pointable a, KnownNat (SizeOf a), IsPrim a ~ f, K1Helper' f (K1 i a)) => GPointable (K1 i a) where
  type GSizeOf (K1 i a) = 'Si (IsPrim a) ('TypeNat a) ('MyNat 8)

  type GIsPrim (K1 i a) = 'False

  gMake :: KnownNat (ToNat (GSizeOf (K1 i a))) => K1 i a p -> IO (Ptr (K1 i a p))
  gMake = k1GMake' (Proxy @f)
  {-# INLINE gMake #-}

  gLoad :: KnownNat (ToNat (GSizeOf (K1 i a))) => Ptr (K1 i a p) -> IO (K1 i a p)
  gLoad = k1GLoad' (Proxy @f)
  {-# INLINE gLoad #-}

  gStore :: KnownNat (ToNat (GSizeOf (K1 i a))) => Ptr (K1 i a p) -> K1 i a p -> IO ()
  gStore = k1GStore' (Proxy @f)
  {-# INLINE gStore #-}

class K1Helper' (f :: Bool) k where
  k1GMake' :: KnownNat (ToNat (GSizeOf k)) => Proxy f -> k p -> IO (Ptr (k p))

  k1GLoad' :: KnownNat (ToNat (GSizeOf k)) => Proxy f -> Ptr (k p) -> IO (k p)

  k1GStore' :: KnownNat (ToNat (GSizeOf k)) => Proxy f -> Ptr (k p) -> k p -> IO ()

instance (Pointable a, KnownNat (SizeOf a)) => K1Helper' 'True (K1 i a) where
  k1GMake' :: (Pointable a, KnownNat (SizeOf a), KnownNat (ToNat (GSizeOf (K1 i a)))) => Proxy 'True -> K1 i a p -> IO (Ptr (K1 i a p))
  k1GMake' _ = fmap castPtr . make . unK1
  {-# INLINE k1GMake' #-}

  k1GLoad' :: (Pointable a, KnownNat (SizeOf a), KnownNat (ToNat (GSizeOf (K1 i a)))) => Proxy 'True -> Ptr (K1 i a p) -> IO (K1 i a p)
  k1GLoad' _ = fmap K1 . load . castPtr
  {-# INLINE k1GLoad' #-}

  k1GStore' :: (Pointable a, KnownNat (SizeOf a), KnownNat (ToNat (GSizeOf (K1 i a)))) => Proxy 'True -> Ptr (K1 i a p) -> K1 i a p -> IO ()
  k1GStore' _ ptr = store (castPtr ptr) . unK1
  {-# INLINE k1GStore' #-}

instance (Pointable a, KnownNat (SizeOf a)) => K1Helper' 'False (K1 i a) where
  k1GMake' :: (Pointable a, KnownNat (SizeOf a), KnownNat (ToNat (GSizeOf (K1 i a)))) => Proxy 'False -> K1 i a p -> IO (Ptr (K1 i a p))
  k1GMake' _ a = do
    ptr <- make (unK1 a)
    ptr' <- mallocBytes 8
    poke ptr' ptr
    pure $ castPtr ptr'
  {-# INLINE k1GMake' #-}

  k1GLoad' :: (Pointable a, KnownNat (SizeOf a), KnownNat (ToNat (GSizeOf (K1 i a)))) => Proxy 'False -> Ptr (K1 i a p) -> IO (K1 i a p)
  k1GLoad' _ ptr = do
    ptr' <- peek (castPtr ptr :: Ptr (Ptr a))
    K1 <$> load ptr'
  {-# INLINE k1GLoad' #-}

  k1GStore' :: (Pointable a, KnownNat (SizeOf a), KnownNat (ToNat (GSizeOf (K1 i a)))) => Proxy 'False -> Ptr (K1 i a p) -> K1 i a p -> IO ()
  k1GStore' _ ptr a = do
    ptr' <- peek (castPtr ptr :: Ptr (Ptr a))
    store ptr' $ unK1 a
  {-# INLINE k1GStore' #-}

instance (GPointable a, GPointable b, KnownNat (ToNat (GSizeOf a)), KnownNat (ToNat (GSizeOf b))) => GPointable (a :*: b) where
  type GSizeOf (a :*: b) = 'MyNat (ToNat (GSizeOf a) + ToNat (GSizeOf b))

  type GIsPrim (a :*: b) = 'False

  gMake :: KnownNat (ToNat (GSizeOf (a :*: b))) => (a :*: b) p -> IO (Ptr ((a :*: b) p))
  gMake a = do
    ptr <- mallocBytes $ gSizeOf a
    gStore ptr a
    pure ptr
  {-# INLINE gMake #-}

  gLoad :: KnownNat (ToNat (GSizeOf (a :*: b))) => Ptr ((a :*: b) p) -> IO ((a :*: b) p)
  gLoad ptr = do
    a <- gLoad $ castPtr ptr
    b <- gLoad $ castPtr ptr `plusPtr` gSizeOf a
    pure $ a :*: b
  {-# INLINE gLoad #-}

  gStore :: KnownNat (ToNat (GSizeOf (a :*: b))) => Ptr ((a :*: b) p) -> (a :*: b) p -> IO ()
  gStore ptr (a :*: b) = do
    gStore (castPtr ptr) a
    gStore (castPtr ptr `plusPtr` gSizeOf a) b
  {-# INLINE gStore #-}


--------------------------------------------------------------------------------
-- Helper Types
--------------------------------------------------------------------------------

data MyNat = MyNat Nat | TypeNat Type | Si Bool MyNat MyNat

type family ToNat (n :: MyNat) :: Nat where
  ToNat ('MyNat n) = n
  ToNat ('TypeNat t) = SizeOf t
  ToNat ('Si 'True a b) = ToNat a
  ToNat ('Si 'False a b) = ToNat b
