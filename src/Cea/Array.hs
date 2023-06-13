module Cea.Array where

import           Control.Monad
import           Cea.Pointer
import           Data.Array
import           Data.Foldable
import           Data.Proxy
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.TypeNats

newtype ArrayOf (n :: Maybe Nat) a = ArrayOf a
  deriving (Eq, Ord, Show)

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

eraseLen :: Ptr (ArrayOf ('Just n) a) -> Ptr (ArrayOf 'Nothing a)
eraseLen = castPtr
{-# INLINE eraseLen #-}

class ArrayLen a where
  arrLen :: a -> IO Int

class ArrayLen a => ArrayPointable n a e | n a -> e where
  loadArr :: n -> a -> IO e

  storeArr :: n -> a -> e -> IO ()

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
    loadArr :: Int -> Ptr (ArrayOf ('Just n) e) -> IO e
    loadArr = loadArrInt
    {-# INLINE loadArr #-}

    storeArr :: Int -> Ptr (ArrayOf ('Just n) e) -> e -> IO ()
    storeArr = storeArrInt
    {-# INLINE storeArr #-}

instance Pointable e => ArrayPointable Int (Ptr (ArrayOf 'Nothing e)) e where
  loadArr :: Int -> Ptr (ArrayOf 'Nothing e) -> IO e
  loadArr = loadArrInt
  {-# INLINE loadArr #-}

  storeArr :: Int -> Ptr (ArrayOf 'Nothing e) -> e -> IO ()
  storeArr = storeArrInt
  {-# INLINE storeArr #-}

instance (KnownNat n, KnownNat n', Pointable e, CmpNat n n' ~ 'LT)
  => ArrayPointable (Proxy n) (Ptr (ArrayOf (Just n') e)) e where
    loadArr :: Proxy n -> Ptr (ArrayOf (Just n') e) -> IO e
    loadArr _ ptr = do
      len <- arrLen ptr
      let ix = fromIntegral $ natVal (Proxy @n)
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      load (ptr `plusPtr` (ix * elemSize))
    {-# INLINE loadArr #-}

    storeArr :: Proxy n -> Ptr (ArrayOf (Just n') e) -> e -> IO ()
    storeArr _ ptr e = do
      len <- arrLen ptr
      let ix = fromIntegral $ natVal (Proxy @n)
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      store (ptr `plusPtr` (ix * elemSize)) e
    {-# INLINE storeArr #-}

instance (KnownNat n, Pointable e)
  => ArrayPointable (Proxy n) (Ptr (ArrayOf 'Nothing e)) e where
    loadArr :: Proxy n -> Ptr (ArrayOf 'Nothing e) -> IO e
    loadArr _ ptr = do
      len <- arrLen ptr
      let ix = fromIntegral $ natVal (Proxy @n)
      if ix >= len || ix < 0
        then error "loadArr: Index out of bound"
        else do
          let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
          load (ptr `plusPtr` (ix * elemSize))
    {-# INLINE loadArr #-}

    storeArr :: Proxy n -> Ptr (ArrayOf 'Nothing e) -> e -> IO ()
    storeArr _ ptr e = do
      len <- arrLen ptr
      let ix = fromIntegral $ natVal (Proxy @n)
      if ix >= len || ix < 0
        then error "storeArr: Index out of bound"
        else do
          let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
          store (ptr `plusPtr` (ix * elemSize)) e
    {-# INLINE storeArr #-}

loadArrInt :: forall e p
           . ( ArrayLen (Ptr (ArrayOf p e))
             , Pointable e )
          => Int -> Ptr (ArrayOf p e) -> IO e
loadArrInt ix ptr = do
  len <- arrLen ptr
  if ix >= len || ix < 0
    then error "loadArr: Index out of bound"
    else do
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      load (ptr `plusPtr` (ix * elemSize))
{-# INLINE loadArrInt #-}

storeArrInt :: forall e p
             . ( ArrayLen (Ptr (ArrayOf p e))
               , Pointable e )
            => Int -> Ptr (ArrayOf p e) -> e -> IO ()
storeArrInt ix ptr e = do
  len <- arrLen ptr
  if ix >= len || ix < 0
    then error "storeArr: Index out of bound"
    else do
      let elemSize = fromIntegral $ val (Proxy @(SizeOf e))
      store (ptr `plusPtr` (ix * elemSize)) e
{-# INLINE storeArrInt #-}
