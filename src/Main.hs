
import qualified Cea.Pointer as Cea
import qualified Cea.Pointer.Unsafe as Unsafe
import           Control.Monad
import           Data.Int
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics

data Foo = Foo Int8 Int8 Int8 Int8
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable Foo

data IntWrapper = IW Int32
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable IntWrapper

data Baz = Baz IntWrapper
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable Baz

data Bar = Bar Foo
  deriving stock (Eq, Ord, Show, Generic)
  deriving Cea.Pointable via Cea.WithPointable Bar

main :: IO ()
main = do
  let foo1 = Foo 11 45 14 19
  let foo2 = Foo 11 45 14 19
  ptr <- Cea.make (Bar foo1)
  let ptr' = castPtr ptr :: Ptr Baz
  val <- Cea.load ptr'
  print val
  print $ Cea.sizeOf $ Bar foo1
