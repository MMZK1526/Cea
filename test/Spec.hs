{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
main = 
  hspec do
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
    it "Can modify fields in CustomTuple" do
      ptr <- make val1
      storesAt @'[0, 0] ptr 4
      loadsAt @'[0, 0] ptr >>= (`shouldBe` 4)
      storesAt @'[0, 1] ptr 5
      loadsAt @'[0, 1] ptr >>= (`shouldBe` 5)
      storesAt @'[0, 2] ptr 1
      loadsAt @'[0, 2] ptr >>= (`shouldBe` 1)
      storesAt @'[0, 3, 0] ptr '8'
      loadsAt @'[0, 3, 0] ptr >>= (`shouldBe` '8')
      storesAt @'[0, 3, 1] ptr 2
      loadsAt @'[0, 3, 1] ptr >>= (`shouldBe` 2)
      storesAt @'[1, 0, 0] ptr 10
      loadsAt @'[1, 0, 0] ptr >>= (`shouldBe` 10)
      storesAt @'[1, 0, 1] ptr 2
      loadsAt @'[1, 0, 1] ptr >>= (`shouldBe` 2)
      storesAt @'[1, 0, 2, 0] ptr 8
      loadsAt @'[1, 0, 2, 0] ptr >>= (`shouldBe` 8)
      storesAt @'[1, 1, 0] ptr ()
      loadsAt @'[1, 1, 0] ptr >>= (`shouldBe` ())
      storesAt @'[1, 1, 1] ptr False
      loadsAt @'[1, 1, 1] ptr >>= (`shouldBe` False)
      storesAt @'[1, 1, 2] ptr 8
      loadsAt @'[1, 1, 2] ptr >>= (`shouldBe` 8)
      storesAt @'[1, 1, 3] ptr 1919
      loadsAt @'[1, 1, 3] ptr >>= (`shouldBe` 1919)
      loadsAt @'[] ptr >>= (`shouldBe` val2)
      delete ptr
  describe "Access by selector name" do
    it "Can access MySolo" do
      ptr <- make (MySolo 'c')
      loadAt @"value1" ptr >>= (`shouldBe` 'c')



primitiveAssertion :: forall a. (Bounded a, Pointable a, Eq a, Show a) => IO ()
primitiveAssertion = do
  ptr <- make (maxBound :: a)
  load ptr >>= (`shouldBe` maxBound)
  store ptr minBound
  load ptr >>= (`shouldBe` minBound)
  delete ptr
