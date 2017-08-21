{-# LANGUAGE GADTs #-}

module Nes (
  -- * Types
    Nes(..)
  , Address(..)
  -- * Functions
  , new
  , load
  , store
  -- , render
) where

import           Control.Monad.ST
import           Data.STRef                  (STRef, modifySTRef', newSTRef,
                                              readSTRef)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Prelude                     hiding (replicate)
import           Util

data Nes s = Nes {
  memory :: VUM.MVector s Word8,
  pc     :: STRef       s Word16,
  sp     :: STRef       s Word8
}

-- GADTs are used to represent addressing
data Address a where
  Pc :: Address Word16
  Sp :: Address Word8
  P :: Address Word8
  X :: Address Word8
  Y :: Address Word8
  Ram8 :: Word16 -> Address Word8
  Ram16 :: Word16 -> Address Word16

new :: ST s (Nes s)
new = do
  mem <- VUM.replicate 65536 0
  pc <- newSTRef 0
  sp <- newSTRef 0xFD
  pure $ Nes mem pc sp

load :: Nes s -> Address a -> ST s a
load nes Pc       = readSTRef (pc nes)
load nes Sp       = readSTRef (sp nes)
load nes (Ram8 r) = VUM.read (memory nes) $ fromIntegral r
load nes (Ram16 r) = do
  let idx = fromIntegral r
  l <- VUM.read (memory nes) idx
  h <- VUM.read (memory nes) (idx + 1)
  pure $ makeW16 l h

store :: Nes s -> Address a -> a -> ST s ()
store nes Pc v       = modifySTRef' (pc nes) (const v)
store nes Sp v       = modifySTRef' (sp nes) (const v)
store nes (Ram8 r) v = VUM.write (memory nes) (fromIntegral r) v
store nes (Ram16 r) v = do
  let idx = fromIntegral r
  let (l, h) = splitW16 v
  VUM.write (memory nes) idx l
  VUM.write (memory nes) (idx + 1) h
