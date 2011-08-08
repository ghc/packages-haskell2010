{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Foreign.Marshal.Error (
  throwIf,       -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO a
  throwIf_,      -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO ()
  throwIfNeg,    -- :: (Ord a, Num a) 
                 -- =>                (a -> String) -> IO a       -> IO a
  throwIfNeg_,   -- :: (Ord a, Num a)
                 -- =>                (a -> String) -> IO a       -> IO ()
  throwIfNull,   -- ::                String        -> IO (Ptr a) -> IO (Ptr a)

  -- Discard return value
  --
  void           -- IO a -> IO ()

  ) where
import "base" Foreign.Marshal.Error
