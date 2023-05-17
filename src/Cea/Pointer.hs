module Cea.Pointer where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Coerce
import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Utils as F
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as F
import           GHC.Generics

class Pointable a where
  size :: a -> Int
  default size :: (Generic a, GPointable (Rep a)) => a -> Int
  size = gSize . from

  make :: a -> IO (Ptr a)
  make a = do
    ptr <- mallocBytes $ size a
    store ptr a
    pure ptr

  load :: Ptr a -> IO a
  default load :: (Generic a, GPointable (Rep a)) => Ptr a -> IO a
  load = fmap to . gLoad . castPtr

  store :: Ptr a -> a -> IO ()
  default store :: (Generic a, GPointable (Rep a)) => Ptr a -> a -> IO ()
  store ptr = gStore (castPtr ptr) . from

instance Pointable Int8 where
  size :: Int8 -> Int
  size = const 1
  {-# INLINE size #-}

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
  size :: Int16 -> Int
  size = const 2
  {-# INLINE size #-}

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
  size :: Int32 -> Int
  size = const 4
  {-# INLINE size #-}

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
  size :: Int64 -> Int
  size = const 8
  {-# INLINE size #-}

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
  size :: Word8 -> Int
  size = const 1
  {-# INLINE size #-}

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
  size :: Word16 -> Int
  size = const 2
  {-# INLINE size #-}

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
  size :: Word32 -> Int
  size = const 4
  {-# INLINE size #-}

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
  size :: Word64 -> Int
  size = const 8
  {-# INLINE size #-}

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
  size :: Int -> Int
  size = const 8
  {-# INLINE size #-}

  make :: Int -> IO (Ptr Int)
  make = fmap castPtr . F.new @Word64 . fromIntegral
  {-# INLINE make #-}

  load :: Ptr Int -> IO Int
  load = fmap fromIntegral . F.peek @Word64 . castPtr
  {-# INLINE load #-}

  store :: Ptr Int -> Int -> IO ()
  store ptr = F.poke (castPtr ptr) . fromIntegral @_ @Word64
  {-# INLINE store #-}

-- We want to know the size of "Char" at compile time, so we will always assume
-- it takes four bytes.
instance Pointable Char where
  size :: Char -> Int
  size = const 4
  {-# INLINE size #-}

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
  size :: () -> Int
  size = const 0
  {-# INLINE size #-}

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
  size :: Float -> Int
  size = const 4
  {-# INLINE size #-}

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
  size :: Double -> Int
  size = const 8
  {-# INLINE size #-}

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
-- Generic Pointable
--------------------------------------------------------------------------------


class GPointable a where
  gSize :: a p -> Int
  gLoad :: Ptr (a p) -> IO (a p)
  gStore :: Ptr (a p) -> a p -> IO ()

instance GPointable U1 where
  gSize :: U1 p -> Int
  gSize = const 0
  {-# INLINE gSize #-}

  gLoad :: Ptr (U1 p) -> IO (U1 p)
  gLoad = const $ pure U1
  {-# INLINE gLoad #-}

  gStore :: Ptr (U1 p) -> U1 p -> IO ()
  gStore = const . const $ pure ()
  {-# INLINE gStore #-}

instance GPointable a => GPointable (M1 i c a) where
  gSize :: M1 i c a p -> Int
  gSize = gSize . unM1
  {-# INLINE gSize #-}

  gLoad :: Ptr (M1 i c a p) -> IO (M1 i c a p)
  gLoad = fmap M1 . gLoad . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (M1 i c a p) -> M1 i c a p -> IO ()
  gStore ptr = gStore (castPtr ptr) . unM1
  {-# INLINE gStore #-}

instance Pointable a => GPointable (K1 i a) where
  gSize :: K1 i a p -> Int
  gSize = size . unK1
  {-# INLINE gSize #-}

  gLoad :: Ptr (K1 i a p) -> IO (K1 i a p)
  gLoad = fmap K1 . load . castPtr
  {-# INLINE gLoad #-}

  gStore :: Ptr (K1 i a p) -> K1 i a p -> IO ()
  gStore ptr = store (castPtr ptr) . unK1
  {-# INLINE gStore #-}

instance (GPointable a, GPointable b) => GPointable (a :*: b) where
  gSize :: (a :*: b) p -> Int
  gSize (a :*: b) = gSize a + gSize b
  {-# INLINE gSize #-}

  gLoad :: Ptr ((a :*: b) p) -> IO ((a :*: b) p)
  gLoad ptr = do
    a <- gLoad $ castPtr ptr
    b <- gLoad $ castPtr ptr `plusPtr` gSize a
    pure $ a :*: b
  {-# INLINE gLoad #-}

  gStore :: Ptr ((a :*: b) p) -> (a :*: b) p -> IO ()
  gStore ptr (a :*: b) = do
    gStore (castPtr ptr) a
    gStore (castPtr ptr `plusPtr` gSize a) b
  {-# INLINE gStore #-}
