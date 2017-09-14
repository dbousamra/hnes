{-# LANGUAGE GADTs #-}

module Emulator.CPU (
    CPU(..)
  , newCPU
) where

import           Control.Monad.ST
import           Data.STRef
import           Data.Word
import           Prelude          hiding (read)

data CPU s = CPU {
  pc :: STRef       s Word16,
  sp :: STRef       s Word8,
  a  :: STRef       s Word8,
  x  :: STRef       s Word8,
  y  :: STRef       s Word8,
  p  :: STRef       s Word8
}

newCPU :: ST s (CPU s)
newCPU = do
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  a <- newSTRef 0x0
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  p <- newSTRef 0x24 -- should this be 0x34?
  pure $ CPU pc sp a x y p
