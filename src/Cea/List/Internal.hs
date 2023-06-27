-- | This module contains the @Pointable@ instance for lists.
--
-- Lists are represented as a pointer to a contiguous block of memory which
-- stores the length of the list followed by the elements.
module Cea.List.Internal where

import           Cea.Array
import           Cea.Pointer.Internal
import           Foreign.Ptr
import           Foreign.Storable

instance Pointable a => Pointable [a] where
  type SizeOf [a] = FromNat PtrSize

  type IsDirect [a] = 'False

  makeInner :: Pointable a => Ptr [a] -> [a] -> IO ()
  makeInner ptr xs = do
    arr <- makeArrFromList xs
    poke (castPtr ptr) arr

  load :: Pointable a => Ptr [a] -> IO [a]
  load ptr = do
    arr <- peek (castPtr ptr) :: IO (Ptr (ArrayOf 'Nothing a))
    loadArrToList arr

  store :: Pointable a => Ptr [a] -> [a] -> IO ()
  store ptr xs = do
    arr <- peek (castPtr ptr) :: IO (Ptr (ArrayOf 'Nothing a))
    deleteArr arr
    makeInner ptr xs

  deleteInner :: Pointable a => Ptr [a] -> IO ()
  deleteInner ptr = do
    arr <- peek (castPtr ptr) :: IO (Ptr (ArrayOf 'Nothing a))
    deleteArr arr
