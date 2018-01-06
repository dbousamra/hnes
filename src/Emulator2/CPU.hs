module Emulator2.CPU (
    CPU(..)
  , new
  , step
  , trace
) where

import           Data.Bits
import           Data.IORef
import qualified Data.Vector.Storable.Mutable as VUM
import           Data.Word
import           Emulator.Opcode
import           Emulator.Util
import           Prelude                      hiding (cycles, read)

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
  , cycles    :: IORef Int
  , interrupt :: IORef (Maybe Interrupt)
  }

step :: CPU -> IO Int
step cpu = do
  -- Start counting the number of cycles.
  -- Some of the opcodes (the branch ones)
  -- modify the cycle count directly due to page crosses
  startingCycles <- readIORef (cycles cpu)
  -- handleInterrupts
  opcode <- loadNextOpcode cpu
  (pageCrossed, addr) <- addressPageCrossForMode cpu (mode opcode)
  addCycles cpu $ getCycles opcode pageCrossed
  incrementPc cpu opcode
  -- runInstruction opcode addr
  endingCycles <- readIORef (cycles cpu)
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

reset :: CPU -> IO ()
reset cpu = do
  v <- readIORef (pc cpu)
  modifyIORef' (pc cpu) (const v)
  modifyIORef' (sp cpu) (const 0xFD)
  modifyIORef' (p cpu) (const 0x24)

read :: CPU -> Word16 -> IO Word8
read cpu addr
  | addr < 0x2000 = readCPURam cpu addr
  -- | addr < 0x4000 = (readPPURegister cpu) addr
  -- | addr == 0x4016 = (readController cpu)
  | addr >= 0x4000 && addr <= 0x4017 = pure 0
  | addr >= 0x4018 && addr <= 0x401F = error "APU read not implemented"
  -- | addr >= 0x6000 = (readMapper cpu) addr
  | otherwise = error "Erroneous read detected!"

read16 :: CPU -> Word16 -> IO Word16
read16 cpu addr = do
  lo <- read cpu addr
  hi <- read cpu (addr + 1)
  pure $ makeW16 lo hi

read16Bug :: CPU -> Word16 -> IO Word16
read16Bug cpu addr = do
  lo <- read cpu addr
  hi <- read cpu $ (addr .&. 0xFF00) .|. (toWord16 $ (toWord8 addr) + 1)
  pure $ makeW16 lo hi

readCPURam :: CPU -> Word16 -> IO Word8
readCPURam cpu addr = VUM.unsafeRead (ram cpu) addr'
  where addr' = fromIntegral addr `mod` 0x0800

loadNextOpcode :: CPU -> IO Opcode
loadNextOpcode cpu = do
  pcv <- readIORef (pc cpu)
  av <- read cpu pcv
  pure $ decodeOpcode av

addressPageCrossForMode :: CPU -> AddressMode -> IO (Bool, Word16)
addressPageCrossForMode cpu mode = case mode of
  Absolute -> do
    pcv <- readIORef (pc cpu)
    addrV <- read16 cpu (pcv + 1)
    pure (False, addrV)
  AbsoluteX -> do
    pcv <- readIORef (pc cpu)
    xv <- readIORef (x cpu)
    v <- read16 cpu (pcv + 1)
    let addrV = v + toWord16 xv
    let pageCrossed = differentPages (addrV - (toWord16 xv)) addrV
    pure (pageCrossed, addrV)
  AbsoluteY -> do
    pcv <- readIORef (pc cpu)
    yv <- readIORef (y cpu)
    v <- read16 cpu (pcv + 1)
    let addrV = v + toWord16 yv
    let pageCrossed = differentPages (addrV - (toWord16 yv)) addrV
    pure (pageCrossed, addrV)
  Accumulator ->
    pure (False, 0)
  Immediate -> do
    pcv <- readIORef (pc cpu)
    pure (False, pcv + 1)
  Implied ->
    pure (False, 0)
  Indirect -> do
    pcv <- readIORef (pc cpu)
    addr <- read16 cpu (pcv + 1)
    yo <- read16Bug cpu addr
    pure (False, yo)
  IndirectIndexed -> do
    pcv <- readIORef (pc cpu)
    yv <- readIORef (y cpu)
    v <- read cpu $ pcv + 1
    addr <- read16Bug cpu $ toWord16 v
    let addrV = addr + toWord16 yv
    let pageCrossed = differentPages (addrV - (toWord16 yv)) addrV
    pure (pageCrossed, addrV)
  IndexedIndirect -> do
    pcv <- readIORef (pc cpu)
    xv <- readIORef (x cpu)
    v <- read cpu $ pcv + 1
    addrV <- read16Bug cpu $ toWord16 (v + xv)
    pure (False, addrV)
  Relative -> do
    pcv <- readIORef (pc cpu)
    offset16 <- read16 cpu (pcv + 1)
    let offset8 = firstNibble offset16
    if offset8 < 0x80 then
      pure (False, pcv + 2 + offset8)
    else
      pure (False, pcv + 2 + offset8 - 0x100)
  ZeroPage -> do
    pcv <- readIORef (pc cpu)
    v <- read cpu (pcv + 1)
    pure (False, toWord16 v)
  ZeroPageX -> do
    pcv <- readIORef (pc cpu)
    xv <- readIORef (x cpu)
    v <- read cpu (pcv + 1)
    pure (False, toWord16 $ v + xv)
  ZeroPageY -> do
    pcv <- readIORef (pc cpu)
    yv <- readIORef (y cpu)
    v <- read cpu (pcv + 1)
    pure (False, toWord16 $ v + yv)

differentPages :: Word16 -> Word16 -> Bool
differentPages a b = (a .&. 0xFF00) /= (b .&. 0xFF00)

getCycles :: Opcode -> Bool -> Int
getCycles opcode pageCrossed = if pageCrossed
  then pageCrossCycles opcode + cyc opcode
  else cyc opcode

addCycles :: CPU -> Int -> IO ()
addCycles cpu n = modifyIORef' (cycles cpu) (+ n)

incrementPc :: CPU -> Opcode -> IO ()
incrementPc cpu opcode = modifyIORef' (pc cpu) (+ instrLength)
  where instrLength = fromIntegral $ (len opcode)

trace :: CPU -> IO String
trace cpu = do
  cycles <- readIORef (cycles cpu)
  pure $ "Cycles = " ++ show cycles
