module Foreign.ForeignPtr (
        -- * Finalised data pointers
          ForeignPtr
        , FinalizerPtr
        , FinalizerEnvPtr

        -- ** Basic operations
        , newForeignPtr
        , newForeignPtr_
        , addForeignPtrFinalizer
        , newForeignPtrEnv
        , addForeignPtrFinalizerEnv
        , withForeignPtr

        -- ** Low-level operations
        , unsafeForeignPtrToPtr
        , touchForeignPtr
        , castForeignPtr

        -- ** Allocating managed memory
        , mallocForeignPtr
        , mallocForeignPtrBytes
        , mallocForeignPtrArray
        , mallocForeignPtrArray0
  ) where

import qualified "base" Foreign.ForeignPtr as Base
import "base" Foreign.ForeignPtr hiding (mallocForeignPtr)
import "base" Foreign (Storable)

-- SDM: local copy of the docs for mallocForeignPtr, to omit the
-- GHC-specific bits.

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
-- ^ Allocate some memory and return a 'ForeignPtr' to it.  The memory
-- will be released automatically when the 'ForeignPtr' is discarded.
--
-- 'mallocForeignPtr' is equivalent to
--
-- >    do { p <- malloc; newForeignPtr finalizerFree p }
-- 
-- although it may be implemented differently internally: you may not
-- assume that the memory returned by 'mallocForeignPtr' has been
-- allocated with 'Foreign.Marshal.Alloc.malloc'.
mallocForeignPtr = Base.mallocForeignPtr
