import qualified Cea.Pointer as Cea
import qualified Cea.Pointer.Accessor as Cea
import           Control.Monad
import           Data.Int
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import Data.Data (Proxy(..))
import Criterion.Main

data Int8Tuple = Int8Tuple Int8 Int8
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable Int8Tuple

data TupleOfInt8Tuples = TupleOfInt8Tuples Int8Tuple Int8Tuple
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable TupleOfInt8Tuples

data PtrTuple = PtrTuple (Ptr ()) (Ptr ())
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable PtrTuple

demoPrimitive :: IO ()
demoPrimitive = do
  putStrLn "Demo Primitive:\n"
  let int32 = 14514 :: Int32
  putStrLn $ "-- Storing " ++ show int32
  ptr <- Cea.make int32
  putStrLn $ "The address is " ++ show ptr ++ ", which may change at each run"
  putStrLn $ "-- Loading the content of " ++ show ptr
  val <- Cea.load ptr -- val == int32
  print val
  -- If we cast the pointer to "Ptr Int8", we would see the truncated value.
  let ptr' = castPtr ptr :: Ptr Int8
  putStrLn $ "-- Loading the content of " ++ show ptr' ++ " as 'Ptr Int8'"
  val' <- Cea.load ptr'
  print val'
  -- If we cast the pointer to "Ptr Char", we would see the corresponding
  -- character.
  let ptr'' = castPtr ptr :: Ptr Char
  putStrLn $ "-- Loading the content of " ++ show ptr'' ++ " as 'Ptr Char'"
  val'' <- Cea.load ptr''
  putStrLn [val'']

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

data IntTuple = IntTuple Int Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable IntTuple

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib' 0 1 2
  where
    fib' :: Int -> Int -> Int -> Int
    fib' a b i
      | i == x = a + b
      | otherwise = b `seq` fib' b (a + b) (i + 1)

fibCea :: Int -> IO Int
fibCea 0 = pure 0
fibCea 1 = pure 1
fibCea n = do
  ptr <- Cea.make $ IntTuple 0 1
  let ptrA = Cea.access (Proxy @0) ptr
      ptrB = Cea.access (Proxy @1) ptr
  replicateM_ (n - 2) $ do
    a <- Cea.load ptrA
    b <- Cea.load ptrB
    Cea.store ptrA b
    Cea.store ptrB (a + b)
  IntTuple a b <- Cea.load ptr
  pure $ a + b

main :: IO ()
main = do
  let n = 10000
  defaultMain
    [ bench "fib" $ whnf fib n
    , bench "fibCea" $ whnfAppIO fibCea n ]
  -- demoPrimitive >> putStrLn ""
  -- demoNestedTuples >> putStrLn ""
