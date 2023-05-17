import qualified Cea.Pointer as Cea
import qualified Cea.Pointer.Unsafe as Unsafe
import           Control.Monad
import           Data.Int
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics

data Foo = Foo Int8 Int8 Int8 Int8 Int8 Int8 Int8 Int8
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Cea.Pointable)

main :: IO ()
main = fore >> back
  where
    fore = do
      ptr <- Cea.make $ Foo 11 45 14 19 19 8 1 0
      let ptr' = castPtr ptr :: Ptr Int64
      val <- Cea.load ptr'
      print val
    back = do
      ptr <- Cea.make (290352993807627 :: Int64)
      let ptr' = castPtr ptr :: Ptr Foo
      val <- Cea.load ptr'
      print val
