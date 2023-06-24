{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

import           Cea.Array
import           Cea.Pointer
import           Cea.Pointer.Accessor
import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           GHC.Generics
import           Test.Hspec

type FancyTuple
  = ((Int8, Int16), Int32, (Int64, Float, Double, ()), Bool, (Char, Word64))

newtype MySolo a = MySolo { value1 :: a }
  deriving (Eq, Show, Generic)
  deriving Pointable via WithPointable (MySolo a)

data MyTuple a b = MyTuple { value1 :: a, value2 :: b }
  deriving (Eq, Show, Generic)
  deriving Pointable via WithPointable (MyTuple a b)

data MyTriple a b c = MyTriple { value1 :: a, value2 :: b, value3 :: c }
  deriving (Eq, Show, Generic)
  deriving Pointable via WithPointable (MyTriple a b c)

data MyQuadruple a b c d
  = MyQuadruple { value1 :: a, value2 :: b, value3 :: c, value4 :: d }
    deriving (Eq, Show, Generic)
    deriving Pointable via WithPointable (MyQuadruple a b c d)

type CustomTuple = MyTuple (MyQuadruple Int8 Int16 Int32 (MyTuple Char Word8))
                           (MyTuple (MyTriple Int64 Float (MySolo Double))
                                    (MyQuadruple () Bool IntPtr WordPtr))

main :: IO ()
main = hspec do
  describe "Primitive type load & store" do
    it "Can make, load, and store Int8" $ primitiveAssertion @Int8
    it "Can make, load, and store Int16" $ primitiveAssertion @Int16
    it "Can make, load, and store Int32" $ primitiveAssertion @Int32
    it "Can make, load, and store Int64" $ primitiveAssertion @Int64
    it "Can make, load, and store Int" $ primitiveAssertion @Int
    it "Can make, load, and store Word8" $ primitiveAssertion @Word8
    it "Can make, load, and store Word16" $ primitiveAssertion @Word16
    it "Can make, load, and store Word32" $ primitiveAssertion @Word32
    it "Can make, load, and store Word64" $ primitiveAssertion @Word64
    it "Can make, load, and store Word" $ primitiveAssertion @Word
    it "Can make, load, and store Char" $ primitiveAssertion @Char
    it "Can make, load, and store Bool" $ primitiveAssertion @Bool
    it "Can make, load, and store ()" $ primitiveAssertion @()
    it "Can make, load, and store Float" do
      ptr <- make (114.514 :: Float)
      load ptr >>= (`shouldBe` 114.514)
      store ptr 1919.810
      load ptr >>= (`shouldBe` 1919.810)
      delete ptr
    it "Can make, load, and store Double" do
      ptr <- make (114.514 :: Double)
      load ptr >>= (`shouldBe` 114.514)
      store ptr 1919.810
      load ptr >>= (`shouldBe` 1919.810)
      delete ptr
    it "Can make, load, and store IntPtr" $ primitiveAssertion @IntPtr
    it "Can make, load, and store WordPtr" $ primitiveAssertion @WordPtr
    it "Can make, load, and store Ptr" $ do
      ptr <- make (nullPtr :: Ptr ())
      val <- load ptr
      val `shouldBe` nullPtr
      store ptr (val `plusPtr` 114514)
      load ptr >>= (`shouldBe` (nullPtr `plusPtr` 114514))
      delete ptr
  
  describe "Native tuple load, store, and index selection" do
    let val1 = ((1, 1), 4, (5, 1, 4, ()), True, ('9', 810)) :: FancyTuple
    let val2 = ((4, 5), 1, (10, 2, 8, ()), False, ('8', 1919)) :: FancyTuple
    it "Can make, load, and store FancyTuple" do
      ptr <- make val1
      load ptr >>= (`shouldBe` val1)
      store ptr val2
      load ptr >>= (`shouldBe` val2)
      delete ptr
    it "Can access fields in FancyTuple" do
      ptr <- make val1
      loadAt @0 ptr >>= (`shouldBe` (1, 1))
      loadsAt @'[0, 0] ptr >>= (`shouldBe` 1)
      loadsAt @'[0, 1] ptr >>= (`shouldBe` 1)
      loadAt @1 ptr >>= (`shouldBe` 4)
      loadAt @2 ptr >>= (`shouldBe` (5, 1, 4, ()))
      loadsAt @'[2, 0] ptr >>= (`shouldBe` 5)
      loadsAt @'[2, 1] ptr >>= (`shouldBe` 1)
      loadsAt @'[2, 2] ptr >>= (`shouldBe` 4)
      loadsAt @'[2, 3] ptr >>= (`shouldBe` ())
      loadAt @3 ptr >>= (`shouldBe` True)
      loadAt @4 ptr >>= (`shouldBe` ('9', 810))
      loadsAt @'[4, 0] ptr >>= (`shouldBe` '9')
    it "Can modify fields in FancyTuple" do
      ptr <- make val1
      storeAt @0 ptr (4, 5)
      loadAt @0 ptr >>= (`shouldBe` (4, 5))
      storeAt @1 ptr 1
      loadAt @1 ptr >>= (`shouldBe` 1)
      storeAt @2 ptr (10, 2, 8, ())
      loadAt @2 ptr >>= (`shouldBe` (10, 2, 8, ()))
      storeAt @3 ptr False
      loadAt @3 ptr >>= (`shouldBe` False)
      storeAt @4 ptr ('8', 1919)
      loadAt @4 ptr >>= (`shouldBe` ('8', 1919))
      storesAt @'[0, 0] ptr 1
      loadsAt @'[0, 0] ptr >>= (`shouldBe` 1)
      storesAt @'[0, 1] ptr 1
      loadsAt @'[0, 1] ptr >>= (`shouldBe` 1)
      storesAt @'[2, 0] ptr 5
      loadsAt @'[2, 0] ptr >>= (`shouldBe` 5)
      storesAt @'[2, 1] ptr 1
      loadsAt @'[2, 1] ptr >>= (`shouldBe` 1)
      storesAt @'[2, 2] ptr 4
      loadsAt @'[2, 2] ptr >>= (`shouldBe` 4)
      storesAt @'[2, 3] ptr ()
      loadsAt @'[2, 3] ptr >>= (`shouldBe` ())
      storesAt @'[4, 0] ptr '9'
      loadsAt @'[4, 0] ptr >>= (`shouldBe` '9')
      storesAt @'[4, 1] ptr 810
      loadsAt @'[4, 1] ptr >>= (`shouldBe` 810)
      delete ptr

  describe "Custom tuple load, store, and index selection" do
    let val1 = MyTuple ( MyQuadruple 1 1 4 (MyTuple '5' 1) )
                       ( MyTuple (MyTriple 5 1 (MySolo 4))
                                 (MyQuadruple () True 9 810) ) :: CustomTuple
    let val2 = MyTuple ( MyQuadruple 4 5 1 (MyTuple '8' 2) )
                       ( MyTuple (MyTriple 10 2 (MySolo 8))
                                 (MyQuadruple () False 8 1919) ) :: CustomTuple
    it "Can make, load, and store CustomTuple" do
      ptr <- make val1
      load ptr >>= (`shouldBe` val1)
      store ptr val2
      load ptr >>= (`shouldBe` val2)
      delete ptr
    it "Can access fields in CustomTuple" do
      ptr <- make val1
      loadsAt @'[0, 0] ptr >>= (`shouldBe` 1)
      loadsAt @'[0, 1] ptr >>= (`shouldBe` 1)
      loadsAt @'[0, 2] ptr >>= (`shouldBe` 4)
      loadsAt @'[0, 3, 0] ptr >>= (`shouldBe` '5')
      loadsAt @'[0, 3, 1] ptr >>= (`shouldBe` 1)
      loadsAt @'[1, 0, 0] ptr >>= (`shouldBe` 5)
      loadsAt @'[1, 0, 1] ptr >>= (`shouldBe` 1)
      loadsAt @'[1, 0, 2, 0] ptr >>= (`shouldBe` 4)
      loadsAt @'[1, 1, 0] ptr >>= (`shouldBe` ())
      loadsAt @'[1, 1, 1] ptr >>= (`shouldBe` True)
      loadsAt @'[1, 1, 2] ptr >>= (`shouldBe` 9)
      loadsAt @'[1, 1, 3] ptr >>= (`shouldBe` 810)
      delete ptr
    it "Can modify fields in CustomTuple" do
      ptr <- make val1
      storesAt @'[0, 0] ptr 4
      storesAt @'[0, 1] ptr 5
      storesAt @'[0, 2] ptr 1
      storesAt @'[0, 3, 0] ptr '8'
      storesAt @'[0, 3, 1] ptr 2
      storesAt @'[1, 0, 0] ptr 10
      storesAt @'[1, 0, 1] ptr 2
      storesAt @'[1, 0, 2, 0] ptr 8
      storesAt @'[1, 1, 0] ptr ()
      storesAt @'[1, 1, 1] ptr False
      storesAt @'[1, 1, 2] ptr 8
      storesAt @'[1, 1, 3] ptr 1919
      loadsAt @'[] ptr >>= (`shouldBe` val2)
      delete ptr

  describe "Access product types by selector name" do
    let val1 = MyTuple ( MyQuadruple 1 1 4 (MyTuple '5' 1) )
                       ( MyTuple (MyTriple 5 1 (MySolo 4))
                                 (MyQuadruple () True 9 810) ) :: CustomTuple
    let val2 = MyTuple ( MyQuadruple 4 5 1 (MyTuple '8' 2) )
                       ( MyTuple (MyTriple 10 2 (MySolo 8))
                                 (MyQuadruple () False 8 1919) ) :: CustomTuple
    it "Can access MySolo" do
      ptr <- make (MySolo 'c')
      loadAt @"value1" ptr >>= (`shouldBe` 'c')
      storeAt @"value1" ptr 'd'
      loadAt @"value1" ptr >>= (`shouldBe` 'd')
    it "Can access fields in CustomTuple" do
      ptr <- make val1
      loadsAt @'["value1", "value1"] ptr >>= (`shouldBe` 1)
      loadsAt @'["value1", "value2"] ptr >>= (`shouldBe` 1)
      loadsAt @'["value1", "value3"] ptr >>= (`shouldBe` 4)
      loadsAt @'["value1", "value4", "value1"] ptr >>= (`shouldBe` '5')
      loadsAt @'["value1", "value4", "value2"] ptr >>= (`shouldBe` 1)
      loadsAt @'["value2", "value1", "value1"] ptr >>= (`shouldBe` 5)
      loadsAt @'["value2", "value1", "value2"] ptr >>= (`shouldBe` 1)
      loadsAt @'["value2", "value1", "value3", "value1"] ptr >>= (`shouldBe` 4)
      loadsAt @'["value2", "value2", "value1"] ptr >>= (`shouldBe` ())
      loadsAt @'["value2", "value2", "value2"] ptr >>= (`shouldBe` True)
      loadsAt @'["value2", "value2", "value3"] ptr >>= (`shouldBe` 9)
      loadsAt @'["value2", "value2", "value4"] ptr >>= (`shouldBe` 810)
    it "Can modify fields in CustomTuple" do
      ptr <- make val1
      storesAt @'["value1", "value1"] ptr 4
      storesAt @'["value1", "value2"] ptr 5
      storesAt @'["value1", "value3"] ptr 1
      storesAt @'["value1", "value4", "value1"] ptr '8'
      storesAt @'["value1", "value4", "value2"] ptr 2
      storesAt @'["value2", "value1", "value1"] ptr 10
      storesAt @'["value2", "value1", "value2"] ptr 2
      storesAt @'["value2", "value1", "value3", "value1"] ptr 8
      storesAt @'["value2", "value2", "value1"] ptr ()
      storesAt @'["value2", "value2", "value2"] ptr False
      storesAt @'["value2", "value2", "value3"] ptr 8
      storesAt @'["value2", "value2", "value4"] ptr 1919
      loadsAt @'[] ptr >>= (`shouldBe` val2)
      delete ptr

  describe "Primitive array lifecycle" do
    it "Can make, read from, and write to Int array with compile-time length" do
      arr  <- makeArr @3 (-1 :: Int)
      list <- loadArrToList arr
      list `shouldBe` replicate 3 (-1)
      e0   <- readArr' @0 arr
      e1   <- readArr' @1 arr
      e2   <- readArr' @2 arr
      list `shouldBe` [e0, e1, e2]
      writeArr' @0 arr 0
      writeArr' @1 arr 1
      writeArr' @2 arr 2
      loadArrToList arr >>= (`shouldBe` [0, 1, 2])
      deleteArr arr
    it "Can make, read from, and write to Int array with run-time length" do
      arr  <- makeArrFromList @Int [-1, -1, -1]
      list <- loadArrToList arr
      list `shouldBe` replicate 3 (-1)
      e0   <- readArr 0 arr
      e1   <- readArr 1 arr
      e2   <- readArr 2 arr
      list `shouldBe` [e0, e1, e2]
      writeArr 0 arr 0
      writeArr 1 arr 1
      writeArr 2 arr 2
      loadArrToList arr >>= (`shouldBe` [0, 1, 2])
      deleteArr arr
    it "Throws error when accessing out-of-bounds index" do
      arr <- makeArrFromList @Int [-1, -1, -1]
      readArr (-1) arr `shouldThrow` anyErrorCall
      writeArr (-1) arr 0 `shouldThrow` anyErrorCall
      readArr 3 arr `shouldThrow` anyErrorCall
      writeArr 3 arr 0 `shouldThrow` anyErrorCall
      deleteArr arr
  
  describe "Custom tuple array lifecycle" do
    let val1 = MyTuple ( MyQuadruple 1 1 4 (MyTuple '5' 1) )
                       ( MyTuple (MyTriple 5 1 (MySolo 4))
                                 (MyQuadruple () True 9 810) ) :: CustomTuple
    let val2 = MyTuple ( MyQuadruple 4 5 1 (MyTuple '8' 2) )
                       ( MyTuple (MyTriple 10 2 (MySolo 8))
                                 (MyQuadruple () False 8 1919) ) :: CustomTuple
    let list = [val1, val2]
    it "Can make, read from, and write to CustomTuple array" do
      arr <- makeArrFromList list
      loadArrToList arr >>= (`shouldBe` list)
      e0 <- readArr 0 arr
      e1 <- readArr 1 arr
      list `shouldBe` [e0, e1]
      e0 `shouldBe` val1
      e1 `shouldBe` val2
      writeArr 0 arr val2
      writeArr 1 arr val1
      loadArrToList arr >>= (`shouldBe` [val2, val1])
      deleteArr arr
    it "Can access element pointrs of CustomTuple array" do
      arr  <- makeArrFromList list
      ptr0 <- accessArr 0 arr
      ptr1 <- accessArr 1 arr
      load ptr1 >>= store ptr0
      loadArrToList arr >>= (`shouldBe` [val2, val2])
      deleteArr arr

primitiveAssertion :: forall a. (Bounded a, Pointable a, Eq a, Show a) => IO ()
primitiveAssertion = do
  ptr <- make (maxBound :: a)
  load ptr >>= (`shouldBe` maxBound)
  store ptr minBound
  load ptr >>= (`shouldBe` minBound)
  delete ptr
