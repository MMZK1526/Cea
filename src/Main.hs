
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

main :: IO ()
main = do
  let int8Tuple1 = Int8Tuple 1 2
      int8Tuple2 = Int8Tuple 3 4
      tupleOfInt8Tuples = TupleOfInt8Tuples int8Tuple1 int8Tuple2
  ptr <- Cea.make tupleOfInt8Tuples
  val <- Cea.load ptr
  print val
