{-# LANGUAGE UndecidableInstances #-}
module Cea.Pointer where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Coerce
import           Data.Int
import           Data.Proxy
import           Data.Word
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Utils as F
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as F
import           GHC.Generics
import           GHC.TypeLits

class Pointable a where
  type SizeOf a :: Nat

  sizeOf :: KnownNat (SizeOf a) => a -> Int
  sizeOf _ = fromIntegral $ natVal (Proxy :: Proxy (SizeOf a))

  make :: KnownNat (SizeOf a) => a -> IO (Ptr a)
  make a = do
    ptr <- mallocBytes (sizeOf a)
    store ptr a
    pure ptr

  load :: KnownNat (SizeOf a) => Ptr a -> IO a

  store :: KnownNat (SizeOf a) => Ptr a -> a -> IO ()

instance Pointable Int8 where
  type SizeOf Int8 = 1

  make :: Int8 -> IO (Ptr Int8)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Int8 -> IO Int8
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Int8 -> Int8 -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Int16 where
  type SizeOf Int16 = 2

  make :: Int16 -> IO (Ptr Int16)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Int16 -> IO Int16
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Int16 -> Int16 -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Int32 where
  type SizeOf Int32 = 4

  make :: Int32 -> IO (Ptr Int32)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Int32 -> IO Int32
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Int32 -> Int32 -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Int64 where
  type SizeOf Int64 = 8

  make :: Int64 -> IO (Ptr Int64)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Int64 -> IO Int64
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Int64 -> Int64 -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Word8 where
  type SizeOf Word8 = 1

  make :: Word8 -> IO (Ptr Word8)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Word8 -> IO Word8
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Word8 -> Word8 -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Word16 where
  type SizeOf Word16 = 2

  make :: Word16 -> IO (Ptr Word16)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Word16 -> IO Word16
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Word16 -> Word16 -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Word32 where
  type SizeOf Word32 = 4

  make :: Word32 -> IO (Ptr Word32)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Word32 -> IO Word32
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Word32 -> Word32 -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Word64 where
  type SizeOf Word64 = 8

  make :: Word64 -> IO (Ptr Word64)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Word64 -> IO Word64
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Word64 -> Word64 -> IO ()
  store = F.poke
  {-# INLINE store #-}

-- We want to know the size of "Int" at compile time, so we will always assume
-- it takes eight bytes.
instance Pointable Int where
  type SizeOf Int = 8

  make :: Int -> IO (Ptr Int)
  make = fmap castPtr . F.new @Word64 . fromIntegral
  {-# INLINE make #-}

  load :: Ptr Int -> IO Int
  load = fmap fromIntegral . F.peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr Int -> Int -> IO ()
  store ptr = F.poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

-- We want to know the size of "Word" at compile time, so we will always assume
-- it takes eight bytes.
instance Pointable Word where
  type SizeOf Word = 8

  make :: Word -> IO (Ptr Word)
  make = fmap castPtr . F.new @Word64 . fromIntegral
  {-# INLINE make #-}

  load :: Ptr Word -> IO Word
  load = fmap fromIntegral . F.peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr Word -> Word -> IO ()
  store ptr = F.poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

-- We want to know the size of "Char" at compile time, so we will always assume
-- it takes four bytes.
instance Pointable Char where
  type SizeOf Char = 4

  make :: Char -> IO (Ptr Char)
  make = fmap castPtr . F.new @Word32 . fromIntegral . ord
  {-# INLINE make #-}

  load :: Ptr Char -> IO Char
  load = fmap (chr . fromIntegral) . F.peek @Word32 . castPtr
  {-# INLINE load #-}

  store :: Ptr Char -> Char -> IO ()
  store ptr = F.poke (castPtr ptr) . fromIntegral @_ @Word32 . ord
  {-# INLINE store #-}

instance Pointable () where
  type SizeOf () = 0

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

  make :: Float -> IO (Ptr Float)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Float -> IO Float
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Float -> Float -> IO ()
  store = F.poke
  {-# INLINE store #-}

instance Pointable Double where
  type SizeOf Double = 8

  make :: Double -> IO (Ptr Double)
  make = F.new
  {-# INLINE make #-}

  load :: Ptr Double -> IO Double
  load = F.peek
  {-# INLINE load #-}

  store :: Ptr Double -> Double -> IO ()
  store = F.poke
  {-# INLINE store #-}

--------------------------------------------------------------------------------
-- Generic Derivation Wrapper
--------------------------------------------------------------------------------

newtype WithPointable a = WithPointable { unWithPointable :: a }

instance (Generic a, GPointable (Rep a)) => Pointable (WithPointable a) where
  type SizeOf (WithPointable a) = GSizeOf (Rep a)

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
  type GSizeOf a :: Nat

  gSizeOf :: KnownNat (GSizeOf a) => a p -> Int
  gSizeOf = const . fromIntegral $ natVal (Proxy @(GSizeOf a))
  {-# INLINE gSizeOf #-}

  gLoad :: KnownNat (GSizeOf a) => Ptr (a p) -> IO (a p)

  gStore :: KnownNat (GSizeOf a) => Ptr (a p) -> a p -> IO ()

instance GPointable U1 where
  type GSizeOf U1 = 0

  gLoad :: Ptr (U1 p) -> IO (U1 p)
  gLoad = const $ pure U1
  {-# INLINE gLoad #-}

  gStore :: Ptr (U1 p) -> U1 p -> IO ()
  gStore = const . const $ pure ()
  {-# INLINE gStore #-}

instance GPointable a => GPointable (M1 i c a) where
  type GSizeOf (M1 i c a) = GSizeOf a

  gLoad :: KnownNat (GSizeOf (M1 i c a)) => Ptr (M1 i c a p) -> IO (M1 i c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: KnownNat (GSizeOf (M1 i c a)) => Ptr (M1 i c a p) -> M1 i c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

instance Pointable a => GPointable (K1 i a) where
  type GSizeOf (K1 i a) = SizeOf a -- TODO: Use ptr

  gLoad :: KnownNat (GSizeOf (K1 i a)) => Ptr (K1 i a p) -> IO (K1 i a p)
  gLoad = fmap K1 . load . castPtr
  {-# INLINE gLoad #-}

  gStore :: KnownNat (GSizeOf (K1 i a)) => Ptr (K1 i a p) -> K1 i a p -> IO ()
  gStore ptr = store (castPtr ptr) . unK1
  {-# INLINE gStore #-}

instance (GPointable a, GPointable b, KnownNat (GSizeOf a), KnownNat (GSizeOf b)) => GPointable (a :*: b) where
  type GSizeOf (a :*: b) = GSizeOf a + GSizeOf b

  gLoad :: KnownNat (GSizeOf (a :*: b)) => Ptr ((a :*: b) p) -> IO ((a :*: b) p)
  gLoad ptr = do
    a <- gLoad $ castPtr ptr
    b <- gLoad $ castPtr ptr `plusPtr` gSizeOf a
    pure $ a :*: b
  {-# INLINE gLoad #-}

  gStore :: Ptr ((a :*: b) p) -> (a :*: b) p -> IO ()
  gStore ptr (a :*: b) = do
    gStore (castPtr ptr) a
    gStore (castPtr ptr `plusPtr` gSizeOf a) b
  {-# INLINE gStore #-}
