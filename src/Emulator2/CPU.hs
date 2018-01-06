{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator2.CPU (
    CPU(..)
  , CPUEmulator(..)
  , runCPUEmulator
  , new
  , step
  , trace
) where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans          (MonadIO, lift)
import           Data.Bits
import           Data.IORef
import qualified Data.Vector.Storable.Mutable as VUM
import           Data.Word
import           Emulator.Opcode
import           Emulator.Util
import           Prelude                      hiding (cycle, cycles, read)

data Interrupt
  = IRQ
  | NMI

data CPU = CPU
  { pc        :: IORef Word16
  , sp        :: IORef Word8
  , a         :: IORef Word8
  , x         :: IORef Word8
  , y         :: IORef Word8
  , p         :: IORef Word8
  , ram       :: VUM.IOVector Word8
  , cycle     :: IORef Int
  , interrupt :: IORef (Maybe Interrupt)
  }

newtype CPUEmulator a = CPUEmulator { unNes :: ReaderT CPU IO a }
  deriving (Monad, Applicative, Functor, MonadIO)

runCPUEmulator :: CPUEmulator a ->  IO a
runCPUEmulator  (CPUEmulator reader) = do
  cpu <- new
  runReaderT reader cpu

{-# INLINE load #-}
load :: (CPU -> IORef a) -> CPUEmulator a
load field = CPUEmulator $ do
  cpu <- ask
  lift $ readIORef $ field cpu

{-# INLINE store #-}
store :: (CPU -> IORef a) -> a -> CPUEmulator ()
store field v = modify field (const v)

{-# INLINE modify #-}
modify :: (CPU -> IORef a) -> (a -> a) -> CPUEmulator ()
modify field v = CPUEmulator $ do
  cpu <- ask
  lift $ modifyIORef' (field cpu) v

step :: CPUEmulator Int
step = do
  -- Start counting the number of cycles.
  -- Some of the opcodes (the branch ones)
  -- modify the cycle count directly due to page crosses
  startingCycles <- load cycle
  -- handleInterrupts
  -- opcode <- loadNextOpcode
  -- (pageCrossed, addr) <- addressPageCrossForMode (mode opcode)
  -- addCycles $ getCycles opcode pageCrossed
  addCycles 1
  -- incrementPc opcode
  -- runInstruction opcode addr
  endingCycles <- load cycle
  pure $ endingCycles - startingCycles

new :: IO CPU
new = do
  pc <- newIORef 0x0
  sp <- newIORef 0xFD
  a <- newIORef 0x0
  x <- newIORef 0x0
  y <- newIORef 0x0
  p <- newIORef 0x24 -- should this be 0x34?
  ram <- VUM.replicate 65536 0x0
  cycles <- newIORef 0
  interrupt <- newIORef Nothing
  pure $ CPU pc sp a x y p ram cycles interrupt

reset :: CPUEmulator ()
reset = do
  v <- load pc
  store pc v
  store sp 0xFD
  store p 0x24

read :: Word16 -> CPUEmulator Word8
read addr
  | addr < 0x2000 = readCPURam addr
  -- | addr < 0x4000 = (readPPURegister cpu) addr
  -- | addr == 0x4016 = (readController cpu)
  | addr >= 0x4000 && addr <= 0x4017 = pure 0
  | addr >= 0x4018 && addr <= 0x401F = error "APU read not implemented"
  -- | addr >= 0x6000 = (readMapper cpu) addr
  | otherwise = error "Erroneous read detected!"

read16 :: Word16 -> CPUEmulator Word16
read16 addr = do
  lo <- read addr
  hi <- read $ addr + 1
  pure $ makeW16 lo hi

read16Bug :: Word16 -> CPUEmulator Word16
read16Bug addr = do
  lo <- read addr
  hi <- read $ (addr .&. 0xFF00) .|. (toWord16 $ (toWord8 addr) + 1)
  pure $ makeW16 lo hi

readCPURam :: Word16 -> CPUEmulator Word8
readCPURam addr = pure 1
-- readCPURam addr = VUM.unsafeRead (ram cpu) addr'
  -- where addr' = fromIntegral addr `mod` 0x0800

loadNextOpcode :: CPUEmulator Opcode
loadNextOpcode = do
  pcv <- load pc
  av <- read pcv
  pure $ decodeOpcode av

addressPageCrossForMode :: AddressMode -> CPUEmulator (Bool, Word16)
addressPageCrossForMode mode = case mode of
  Absolute -> do
    pcv <- load pc
    addrV <- read16 $ pcv + 1
    pure (False, addrV)
  AbsoluteX -> do
    pcv <- load pc
    xv <- load x
    v <- read16 $ pcv + 1
    let addrV = v + toWord16 xv
    let pageCrossed = differentPages (addrV - (toWord16 xv)) addrV
    pure (pageCrossed, addrV)
  AbsoluteY -> do
    pcv <- load pc
    yv <- load y
    v <- read16 $ pcv + 1
    let addrV = v + toWord16 yv
    let pageCrossed = differentPages (addrV - (toWord16 yv)) addrV
    pure (pageCrossed, addrV)
  Accumulator ->
    pure (False, 0)
  Immediate -> do
    pcv <- load pc
    pure (False, pcv + 1)
  Implied ->
    pure (False, 0)
  Indirect -> do
    pcv <- load pc
    addr <- read16 $ pcv + 1
    yo <- read16Bug addr
    pure (False, yo)
  IndirectIndexed -> do
    pcv <- load pc
    yv <- load y
    v <- read $ pcv + 1
    addr <- read16Bug $ toWord16 v
    let addrV = addr + toWord16 yv
    let pageCrossed = differentPages (addrV - (toWord16 yv)) addrV
    pure (pageCrossed, addrV)
  IndexedIndirect -> do
    pcv <- load pc
    xv <- load x
    v <- read $ pcv + 1
    addrV <- read16Bug $ toWord16 (v + xv)
    pure (False, addrV)
  Relative -> do
    pcv <- load pc
    offset16 <- read16 $ pcv + 1
    let offset8 = firstNibble offset16
    if offset8 < 0x80 then
      pure (False, pcv + 2 + offset8)
    else
      pure (False, pcv + 2 + offset8 - 0x100)
  ZeroPage -> do
    pcv <- load pc
    v <- read $ pcv + 1
    pure (False, toWord16 v)
  ZeroPageX -> do
    pcv <- load pc
    xv <- load x
    v <- read $ pcv + 1
    pure (False, toWord16 $ v + xv)
  ZeroPageY -> do
    pcv <- load pc
    yv <- load y
    v <- read $ pcv + 1
    pure (False, toWord16 $ v + yv)

differentPages :: Word16 -> Word16 -> Bool
differentPages a b = (a .&. 0xFF00) /= (b .&. 0xFF00)

getCycles :: Opcode -> Bool -> Int
getCycles opcode pageCrossed = if pageCrossed
  then pageCrossCycles opcode + cycles opcode
  else cycles opcode

addCycles :: Int -> CPUEmulator ()
addCycles n = modify cycle (+ n)

incrementPc :: Opcode -> CPUEmulator ()
incrementPc opcode = modify pc (+ instrLength)
  where instrLength = fromIntegral $ (len opcode)

trace :: CPUEmulator String
trace = do
  cycles <- load cycle
  pure $ "Cycles = " ++ show cycles
