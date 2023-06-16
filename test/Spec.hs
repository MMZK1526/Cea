{-# LANGUAGE BlockArguments #-}

import           Cea.Pointer
import           Data.Int
import           Test.Hspec

main :: IO ()
main = hspec do
  describe "Primitive type load & store" do
    it "Can make, load, and store Int8" $ do
      ptr <- make (0 :: Int8)
      load ptr >>= (`shouldBe` 0)
      store ptr 1
      load ptr >>= (`shouldBe` 1)
