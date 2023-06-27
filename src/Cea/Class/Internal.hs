module Cea.Class.Internal where 

import           Data.Int
import           Data.Word
import           Foreign.Ptr

-- | A dummy class for deriving @Pointable@ instances.
class Cea a

instance Cea Word8

instance Cea Word16

instance Cea Word32

instance Cea Word64

instance Cea Word

instance Cea Int8

instance Cea Int16

instance Cea Int32

instance Cea Int64

instance Cea Int

instance Cea Float

instance Cea Double

instance Cea Char

instance Cea Bool

instance Cea ()

instance Cea (Ptr a)

instance Cea IntPtr

instance Cea WordPtr

instance (Cea a, Cea b) => Cea (a, b)

instance (Cea a, Cea b, Cea c) => Cea (a, b, c)

instance (Cea a, Cea b, Cea c, Cea d) => Cea (a, b, c, d)

instance (Cea a, Cea b, Cea c, Cea d, Cea e) => Cea (a, b, c, d, e)
