module Nes (
  -- * Types
    Nes(..)
  , Address(..)
  -- * Functions
  , new
  , readMemory
  , writeMemory
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
  memory :: VUM.MVector s Word8,
  xReg   :: STRef       s Word8
}

data Register
  = Pc
  | Sp
  | P
  | A
  | X
  | Y
  deriving (Eq)

instance Show Register where
  show Pc = "Pc"
  show Sp = "Sp"
  show P  = "Status"
  show A  = "A"
  show X  = "X"
  show Y  = "Y"

data Address
  = Address Word16
  deriving (Eq)

instance Show Address where
  show (Address r) = "[" ++ prettifyWord16 r ++ "]"

fromAddress :: Address -> Int
fromAddress (Address a) =  fromIntegral a

fromRegister :: Register -> (Nes s -> STRef s Word8)
fromRegister reg = case reg of
  X -> xReg
  _ -> undefined

new :: ST s (Nes s)
new = do
  mem <- VUM.replicate 65536 0
  x <- newSTRef 0
  pure $ Nes mem x

readMemory :: Nes s -> Address -> ST s Word8
readMemory (Nes mem _) = VUM.read mem . fromAddress

writeMemory :: Nes s -> Address -> Word8 -> ST s ()
writeMemory (Nes mem _) = VUM.write mem . fromAddress

setRegister :: Nes s -> Register -> Word8 -> ST s ()
setRegister nes reg value = modifySTRef' (fromRegister reg nes) (const value)

readRegister :: Nes s -> Register -> ST s Word8
readRegister nes reg = readSTRef $ fromRegister reg nes

render :: Nes s -> ST s String
render mem = unlines <$> mapM line [(x * 8, x * 8 + 7) | x <- [0 .. 0xffff `div` 8]]
  where
    line (lo, up) = do
      vs <- mapM (readMemory mem . Address) [lo .. up]
      return $ prettifyWord16 lo ++ ": " ++ unwords (map show vs)

