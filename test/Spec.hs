{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

import           Cea.Pointer
import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           GHC.Generics
import           Test.Hspec

type FancyTuple
  = ((Int8, Int16), Int32, (Int64, Float, Double, ()), Bool, (Char, Word64))

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

type CustumTuple = MyTuple (MyQuadruple Int8 Int16 Int32 (MyTuple Char Word8))
                           (MyTuple (MyTriple Int64 Float Double)
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
    it "Can make, load, and store Double" do
      ptr <- make (114.514 :: Double)
      load ptr >>= (`shouldBe` 114.514)
      store ptr 1919.810
      load ptr >>= (`shouldBe` 1919.810)
    it "Can make, load, and store IntPtr" $ primitiveAssertion @IntPtr
    it "Can make, load, and store WordPtr" $ primitiveAssertion @WordPtr
    it "Can make, load, and store Ptr" $ do
      ptr <- make (nullPtr :: Ptr ())
      val <- load ptr
      val `shouldBe` nullPtr
      store ptr (val `plusPtr` 114514)
      load ptr >>= (`shouldBe` (nullPtr `plusPtr` 114514))

primitiveAssertion :: forall a. (Bounded a, Pointable a, Eq a, Show a) => IO ()
primitiveAssertion = do
  ptr <- make (maxBound :: a)
  load ptr >>= (`shouldBe` maxBound)
  store ptr minBound
  load ptr >>= (`shouldBe` minBound)
