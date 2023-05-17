
import qualified Cea.Pointer as Cea
import qualified Cea.Pointer.Unsafe as Unsafe
import           Control.Monad
import           Data.Int
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics

data Foo = Foo Int8 Int8 Int8 Int8 Int8 Int8 Int8 Int8
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable Foo

data Bar = Bar Foo Foo
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable Bar

main :: IO ()
main = do
  let foo1 = Foo 11 45 14 19 19 8 1 0
  ptr <- Cea.make (290352993807627 :: Int64)
  let ptr' = castPtr ptr :: Ptr Foo
  foo2 <- Cea.load ptr'
  ptr <- Cea.make (Bar foo1 foo2)
  val <- Cea.load ptr
  print $ Cea.sizeOf val
  print val
