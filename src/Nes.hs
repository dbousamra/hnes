module Nes (
  -- * Types
    Nes(..)
  , Address(..)
  -- * Functions
  , new
  , load
  , store
  , render
) where

import           Control.Monad.ST
import           Data.STRef                  (STRef, modifySTRef', newSTRef,
                                              readSTRef)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Prelude                     hiding (replicate)
import           Util

data Nes s = Nes {
  memory :: VUM.MVector s Word8
}

data Address
  = Pc
  | Sp
  | P
  | A
  | X
  | Y
  | Ram Word16
  deriving (Eq)

instance Show Address where
  show Pc      = "Pc"
  show Sp      = "Sp"
  show P       = "Status"
  show A       = "A"
  show X       = "X"
  show Y       = "Y"
  show (Ram r) = "[" ++ prettifyWord16 r ++ "]"

fromAddress :: Address -> Int
fromAddress addr = case addr of
  Sp      -> offset + 1
  P       -> offset + 2
  A       -> offset + 4
  X       -> offset + 5
  Y       -> offset + 6
  Pc      -> offset + 6
  (Ram a) -> fromIntegral a
  where offset = 65536

new :: ST s (Nes s)
new = do
  mem <- VUM.replicate 65536 0
  pure $ Nes mem

load :: Nes s -> Address -> ST s Word8
load (Nes mem) = VUM.read mem . fromAddress

store :: Nes s -> Address -> Word8 -> ST s ()
store (Nes mem) = VUM.write mem . fromAddress

render :: Nes s -> ST s String
render mem = unlines <$> mapM line [(x * 8, x * 8 + 7) | x <- [0 .. 0xffff `div` 8]]
  where
    line (lo, up) = do
      vs <- mapM (load mem . Ram) [lo .. up]
      return $ prettifyWord16 lo ++ ": " ++ unwords (map show vs)
