{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module provides functions to work with arrays of @Pointable@ types.
--
-- The array always has its length stored at the first @ptrSize@ bytes of the
-- pointer, and the elements are stored after that.
module Cea.Array
  ( ArrayOf
  , makeArr
  , makeArrFromList
  , loadArr
  , loadArrToList
  , deleteArr
  , eraseLen
  , readArr
  , writeArr
  , readArr'
  , writeArr'
  , arrLen
  , ArrayPointable
  ) where

import           Control.Monad
import           Cea.Pointer
import           Data.Array
import           Data.Array.IO
import           Data.Foldable
import           Data.Kind
import           Data.Proxy
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.TypeNats

-- | A dummy type that represents an array of type @a@ with length @n@. If the
-- length is @'Nothing@, it means that we do not know the length of the array
-- at compile time.
--
-- It is generally used as argument to @Ptr@, e.g. @Ptr (ArrayOf ('Just 3) Int)@
-- which represents a pointer to an array of 3 @Int@s.
data ArrayOf (n :: Maybe Nat) (a :: Type)

-- | Make an array of size @n@ with all elements filled with @x@.
makeArr :: forall n e t
         . ( KnownNat n
           , Pointable e )
        => e -> IO (Ptr (ArrayOf ('Just n) e))
makeArr x = do
  let len      = fromIntegral $ natVal (Proxy @n)
  let elemSize = size x
  ptr <- mallocBytes (ptrSize + len * elemSize)
  let arr      = castPtr ptr `plusPtr` ptrSize
  poke (castPtr ptr) len
  forM_ [0..(len - 1)] \ix -> store (arr `plusPtr` (ix * elemSize)) x
  pure ptr

-- | Make an array pointer from the given list or @Foldable@ instance.
--
-- Since the length of the list is in general unknown at compile time, so is
-- the length of the array.
makeArrFromList :: forall e t
                 . ( Pointable e
                   , Foldable t )
                => t e -> IO (Ptr (ArrayOf 'Nothing e))
makeArrFromList xs = do
  let xs'      = toList xs
  let len      = length xs'
  let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
  ptr <- mallocBytes (ptrSize + len * elemSize)
  let arr      = castPtr ptr `plusPtr` ptrSize
  poke (castPtr ptr) len
  zipWithM_ store (map (arr `plusPtr`) [0, elemSize..]) xs'
  pure ptr

-- | Load the array pointer into an immutable array.
loadArr :: forall e p
         . ( ArrayLen (Ptr (ArrayOf p e))
           , Pointable e )
        => Ptr (ArrayOf p e) -> IO (Array Int e)
loadArr ptr = do
  len <- arrLen ptr
  let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
  arrIO <- newArray_ (0, len - 1) :: IO (IOArray Int e)
  let arr = ptr `plusPtr` ptrSize
  forM_ [0..(len - 1)] $ \ix -> do
    val <- load (arr `plusPtr` (ix * elemSize))
    writeArray arrIO ix val
  freeze arrIO

-- | Load the array pointer into a list.
loadArrToList :: forall e p
               . ( ArrayLen (Ptr (ArrayOf p e))
                 , Pointable e )
              => Ptr (ArrayOf p e) -> IO [e]
loadArrToList ptr = do
  len <- arrLen ptr
  let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
  let arr = ptr `plusPtr` ptrSize
  forM [0..(len - 1)] $ \ix -> load (arr `plusPtr` (ix * elemSize))

-- | Free the array pointer, recursively freeing all its elements.
--
-- It is assumed that all elements of the array "belongs" to the array, in other
-- words, their lifecycles are tied with the array and not managed elsewhere.
deleteArr :: forall e p
           . ( ArrayLen (Ptr (ArrayOf p e))
             , Pointable e )
        => Ptr (ArrayOf p e) -> IO ()
deleteArr ptr = do
  len <- arrLen ptr
  let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
  let arr = castPtr ptr `plusPtr` ptrSize
  forM_ [0..(len - 1)] $ \ix -> deleteInner @e (arr `plusPtr` (ix * elemSize))
  free ptr

-- | Erase the complie-time length of the array pointer.
eraseLen :: Ptr (ArrayOf ('Just n) a) -> Ptr (ArrayOf 'Nothing a)
eraseLen = castPtr
{-# INLINE eraseLen #-}

-- | Read the element at the given index. An error is thrown if the index is
-- out of bounds.
readArr :: ArrayPointable Int a e => Int -> a -> IO e
readArr = readArr_
{-# INLINE readArr #-}

-- | Write the element at the given index. An error is thrown if the index is
-- out of bounds.
writeArr :: ArrayPointable Int a e => Int -> a -> e -> IO ()
writeArr = writeArr_
{-# INLINE writeArr #-}

-- | Read the element at the given index specified by the type level @Nat@ @n@.
-- It only works if the length of the array is known at compile time, and it is
-- a compile time error if the index is out of bounds.
--
-- > do
-- >   arr <- makeArr @3 (0 :: Int)
-- >   e2  <- readArr' @2 arr
-- >   print e2 -- 0
readArr' :: forall (n :: Nat) a e
          . ArrayPointable (Proxy n) a e
         => a -> IO e
readArr' = readArr_ (Proxy @n)

-- | Write the element at the given index specified by the type level @Nat@ @n@.
-- It only works if the length of the array is known at compile time, and it is
-- a compile time error if the index is out of bounds.
--
-- > do
-- >   arr <- makeArr @3 (0 :: Int)
-- >   writeArr' @2 arr 1
-- >   e2  <- readArr' @2 arr
-- >   print e2 -- 1
writeArr' :: forall (n :: Nat) a e
           . ArrayPointable (Proxy n) a e
          => a -> e -> IO ()
writeArr' = writeArr_ (Proxy @n)

-- | Get the length of the array pointer.
class ArrayLen a where
  arrLen :: a -> IO Int

-- | Helper class that allows reading and writing to an array pointer. The
-- methods are not exposed and are only used internally.
class ArrayLen a => ArrayPointable n a e | n a -> e where
  readArr_ :: n -> a -> IO e

  writeArr_ :: n -> a -> e -> IO ()


--------------------------------------------------------------------------------
-- Internal Instances
--------------------------------------------------------------------------------

instance KnownNat n => ArrayLen (Ptr (ArrayOf ('Just n) e)) where
  arrLen :: Ptr (ArrayOf ('Just n) e) -> IO Int
  arrLen _ = pure $ fromIntegral $ natVal (Proxy @n)
  {-# INLINE arrLen #-}

instance ArrayLen (Ptr (ArrayOf 'Nothing e)) where
  arrLen :: Ptr (ArrayOf 'Nothing e) -> IO Int
  arrLen ptr = peek (castPtr ptr)
  {-# INLINE arrLen #-}

instance (KnownNat n, Pointable e)
  => ArrayPointable Int (Ptr (ArrayOf ('Just n) e)) e where
    readArr_ :: Int -> Ptr (ArrayOf ('Just n) e) -> IO e
    readArr_ = readArrInt
    {-# INLINE readArr_ #-}

    writeArr_ :: Int -> Ptr (ArrayOf ('Just n) e) -> e -> IO ()
    writeArr_ = writeArrInt
    {-# INLINE writeArr_ #-}

instance Pointable e => ArrayPointable Int (Ptr (ArrayOf 'Nothing e)) e where
  readArr_ :: Int -> Ptr (ArrayOf 'Nothing e) -> IO e
  readArr_ = readArrInt
  {-# INLINE readArr_ #-}

  writeArr_ :: Int -> Ptr (ArrayOf 'Nothing e) -> e -> IO ()
  writeArr_ = writeArrInt
  {-# INLINE writeArr_ #-}

instance (KnownNat n, KnownNat n', Pointable e, CmpNat n n' ~ 'LT)
  => ArrayPointable (Proxy n) (Ptr (ArrayOf (Just n') e)) e where
    readArr_ :: Proxy n -> Ptr (ArrayOf (Just n') e) -> IO e
    readArr_ _ ptr = do
      len <- arrLen ptr
      let ix = fromIntegral $ natVal (Proxy @n)
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      load (ptr `plusPtr` ptrSize `plusPtr` (ix * elemSize))
    {-# INLINE readArr_ #-}

    writeArr_ :: Proxy n -> Ptr (ArrayOf (Just n') e) -> e -> IO ()
    writeArr_ _ ptr e = do
      len <- arrLen ptr
      let ix = fromIntegral $ natVal (Proxy @n)
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      store (ptr `plusPtr` ptrSize `plusPtr` (ix * elemSize)) e
    {-# INLINE writeArr_ #-}

readArrInt :: forall e p
           . ( ArrayLen (Ptr (ArrayOf p e))
             , Pointable e )
          => Int -> Ptr (ArrayOf p e) -> IO e
readArrInt ix ptr = do
  len <- arrLen ptr
  if ix >= len || ix < 0
    then error "readArr: Index out of bound"
    else do
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      load (ptr `plusPtr` ptrSize `plusPtr` (ix * elemSize))
{-# INLINE readArrInt #-}

writeArrInt :: forall e p
             . ( ArrayLen (Ptr (ArrayOf p e))
               , Pointable e )
            => Int -> Ptr (ArrayOf p e) -> e -> IO ()
writeArrInt ix ptr e = do
  len <- arrLen ptr
  if ix >= len || ix < 0
    then error "writeArr: Index out of bound"
    else do
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      store (ptr `plusPtr` ptrSize `plusPtr` (ix * elemSize)) e
{-# INLINE writeArrInt #-}
