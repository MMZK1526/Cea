
import qualified Cea.Pointer as Cea
import qualified Cea.Pointer.Unsafe as Unsafe
import           Control.Monad
import           Data.Int
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics

data Int8Tuple = Int8Tuple Int8 Int8
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable Int8Tuple

data TupleOfInt8Tuples = TupleOfInt8Tuples Int8Tuple Int8Tuple
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable TupleOfInt8Tuples

data PtrTuple = PtrTuple (Ptr ()) (Ptr ())
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable PtrTuple

demoNestedTuples :: IO ()
demoNestedTuples = do
  putStrLn "Demo Nested Tuples:\n"
  let int8Tuple1 = Int8Tuple 1 2
      int8Tuple2 = Int8Tuple 3 4
      tupleOfInt8Tuples = TupleOfInt8Tuples int8Tuple1 int8Tuple2
  putStrLn $ "-- Storing " ++ show tupleOfInt8Tuples
  -- Create a pointer that stores this nested tuple.
  -- Since the fields of "TupleOfInt8Tuples" are "Int8Tuple"s and this is a
  -- non-primitive type, they will be stored as pointers with size of 8 bytes.
  -- Within each "Int8Tuple", the fields are primitive types, so they will be
  -- stored as values, each taking 2 bytes (1 for each "Int8").
  ptr <- Cea.make tupleOfInt8Tuples
  putStrLn $ "The address is " ++ show ptr ++ ", which may change at each run"
  putStrLn $ "-- Loading the content of " ++ show ptr
  val <- Cea.load ptr -- val == tupleOfInt8Tuples
  print val
  -- | If we cast the pointer to "PtrTuple", since the fields are pointers,
  -- we would see the address of the "Int8Tuple"s rather some representation of
  --  their values.
  let ptr' = castPtr ptr :: Ptr PtrTuple
  putStrLn $ "-- Loading the content of " ++ show ptr' ++ " as 'PtrTuple'"
  val' <- Cea.load ptr'
  print val'

main :: IO ()
main = demoNestedTuples
