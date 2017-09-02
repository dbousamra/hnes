module Emulator (
  -- * Functions
    run
  , r
) where

import           Cartridge
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              (clearBit, setBit, testBit, (.&.),
                                         (.|.))
import           Data.ByteString        as BS hiding (putStrLn, replicate, take,
                                               zip)
import           Data.Word
import           Monad
import           Nes                    (Address (..), Flag (..))
import           Opcode
import           Util

data EmulatorState
  = Continue
  | Fail
  deriving (Eq)

r :: IO ()
r = void $ runDebug "roms/nestest.nes" 0xC000 (pure Continue)

run :: FilePath -> IO ()
run fp = do
  cart <- parseCartridge <$> BS.readFile fp
  runIOEmulator cart $ void $ emulate $ pure Continue

runDebug :: FilePath -> Word16 -> Monad.IOEmulator EmulatorState -> IO EmulatorState
runDebug fp start hook = do
  cart <- parseCartridge <$> BS.readFile fp
  runIOEmulator cart $ do
    store Pc start
    emulate hook

loadNextOpcode :: (MonadIO m, MonadEmulator m) => m Opcode
loadNextOpcode = do
  pc <- load Pc
  pcv <- load (Ram8 pc)
  pure $ decodeOpcode pcv

emulate :: (MonadIO m, MonadEmulator m) => m EmulatorState -> m EmulatorState
emulate hook = do
  opcode <- loadNextOpcode
  execute opcode
  hookRes <- hook
  case hookRes of
    Continue -> emulate hook
    Fail     -> pure Fail

incrementPc :: MonadEmulator m => Word16 -> m ()
incrementPc n = do
  pc <- load Pc
  store Pc (pc + n)

addressForMode :: (MonadIO m, MonadEmulator m) => AddressMode -> m Word16
addressForMode mode = case mode of
  Absolute -> do
    pcv <- load Pc
    load $ Ram16 (pcv + 1)
  AbsoluteX -> do
    pcv <- load Pc
    xv <- load X
    v <- load $ Ram16 (pcv + 1)
    pure $ v + (toWord16 xv)
  Immediate -> do
    pcv <- load Pc
    pure $ pcv + 1
  Implied ->
    pure $ toWord16 0
  Relative -> do
    pcv <- load Pc
    offset16 <- load $ Ram16 (pcv + 1)
    let offset8 = firstNibble offset16
    if offset8 < 0x80 then
      pure $ pcv + 2 + offset8
    else
      pure $ pcv + 2 + offset8 - 0x100
  ZeroPage -> do
    pcv <- load Pc
    v <- load $ Ram8 (pcv + 1)
    pure $ toWord16 v
  other -> error $ "Unimplemented AddressMode " ++ (show other)

pcIncrementForOpcode :: Opcode -> Word16
pcIncrementForOpcode (Opcode _ mn mode) = case (mode, mn) of
  -- (_, JMP)             -> 0
  -- (_, RTS)             -> 0
  -- (_, RTI)             -> 0
  (Indirect, _)        -> 0
  (Relative, _)        -> 2
  (Accumulator, _)     -> 1
  (Implied, _)         -> 1
  (Immediate, _)       -> 2
  (IndexedIndirect, _) -> 2
  (IndirectIndexed, _) -> 2
  (ZeroPage, _)        -> 2
  (ZeroPageX, _)       -> 2
  (ZeroPageY, _)       -> 2
  (Absolute, _)        -> 3
  (AbsoluteX, _)       -> 3
  (AbsoluteY, _)       -> 3

execute :: (MonadIO m, MonadEmulator m) => Opcode -> m ()
execute op @ (Opcode _ mn mode) = do
  addr <- addressForMode mode
  line <- renderExecution op addr
  trace line
  incrementPc $ pcIncrementForOpcode op
  go addr
  where
    go = case mn of
      BCC     -> bcc
      BCS     -> bcs
      BEQ     -> beq
      BIT     -> bit
      BMI     -> bmi
      BNE     -> bne
      BPL     -> bpl
      BVC     -> bvc
      BVS     -> bvs
      CLC     -> const clc
      INC     -> inc
      JMP     -> jmp
      JSR     -> jsr
      LDA     -> lda
      LDX     -> ldx
      NOP     -> const nop
      RTS     -> const rts
      SEC     -> const sec
      STA     -> sta
      STX     -> stx
      STY     -> sty
      unknown -> error $ "Unimplemented opcode: " ++ (show unknown)

pull :: (MonadIO m, MonadEmulator m) => m Word8
pull = do
  spv <- load Sp
  store Sp (spv + 1)
  let i = 0x100 .|. (toWord16 spv + 1)
  load $ Ram8 i

pull16 :: (MonadIO m, MonadEmulator m) => m Word16
pull16 = do
  lo <- pull
  hi <- pull
  pure $ makeW16LittleEndian lo hi

push :: (MonadIO m, MonadEmulator m) => Word8 -> m ()
push v = do
  spv <- load Sp
  let i = 0x100 .|. (toWord16 spv)
  store (Ram8 i) v
  store Sp (spv - 1)

push16 :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
push16 v = do
  let (lo, hi) = splitW16 v
  push hi
  push lo

-- BCC - Branch on carry flag clear
bcc :: MonadEmulator m => Word16 -> m ()
bcc = branch $ not <$> (load $ P FC)

-- BCS - Branch on carry flag set
bcs :: MonadEmulator m => Word16 -> m ()
bcs = branch $ (load $ P FC)

-- BEQ - Branch if zero set
beq :: MonadEmulator m => Word16 -> m ()
beq = branch $ (load $ P FZ)

-- BMI - Branch if minus
bmi :: MonadEmulator m => Word16 -> m ()
bmi = branch $ (load $ P FN)

-- BPL - Branch if positive
bpl :: MonadEmulator m => Word16 -> m ()
bpl = branch $ not <$> (load $ P FN)

-- BVS - Branch if overflow clear
bvc :: MonadEmulator m => Word16 -> m ()
bvc = branch $ not <$> (load $ P FV)

-- BVS - Branch if overflow set
bvs :: MonadEmulator m => Word16 -> m ()
bvs = branch $ (load $ P FV)

-- BIT -
bit :: MonadEmulator m => Word16 -> m ()
bit addr = do
  v <- load $ Ram8 addr
  av <- load A
  let res = (v .&. av)
  setZ res
  setV v
  setN v

-- BNE - Branch if zero not set
bne :: MonadEmulator m => Word16 -> m ()
bne = branch $ not <$> (load $ P FZ)

-- CLC - Clear carry flag
clc :: MonadEmulator m => m ()
clc = store (P FC) False

-- INC - Increment memory
inc :: MonadEmulator m => Word16 -> m ()
inc addr = do
  v <- load $ Ram8 addr
  let value = v + 1
  store (Ram8 addr) value
  setZN value

-- JMP - Move execution to a particular address
jmp :: MonadEmulator m => Word16 -> m ()
jmp = store Pc

-- JSR - Jump to subroutine
jsr :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
jsr addr = do
  pcv <- load Pc
  push16 $ pcv - 1
  store Pc addr

-- LDA - Load accumulator register
lda :: MonadEmulator m => Word16 -> m ()
lda addr = do
  v <- load $ Ram8 addr
  store A v
  setZN v

-- LDX - Load X Register
ldx :: MonadEmulator m => Word16 -> m ()
ldx addr = do
  v <- load $ Ram8 addr
  store X v
  setZN v

-- NOP - No operation. Do nothing :D
nop :: MonadEmulator m => m ()
nop = pure ()

-- RTS - Return from a subroutine
rts :: (MonadIO m, MonadEmulator m) => m ()
rts = do
  addr <- pull16
  store Pc (addr + 1)

-- SEC - Set carry flag
sec :: MonadEmulator m => m ()
sec = store (P FC) True

-- STA - Store Accumulator register
sta :: MonadEmulator m => Word16 -> m ()
sta addr = (load A) >>= (store $ Ram8 addr)

-- STX - Store X register
stx :: MonadEmulator m => Word16 -> m ()
stx addr = (load X) >>= (store $ Ram8 addr)

-- STY - Store Y register
sty :: MonadEmulator m => Word16 -> m ()
sty addr = (load Y) >>= (store $ Ram8 addr)

-- Moves execution to addr if condition is set
branch :: MonadEmulator m => (m Bool) -> Word16 -> m ()
branch cond addr = do
  c <- cond
  if c then
    store Pc addr
  else
    pure ()

-- Sets the zero flag
setZ :: MonadEmulator m => Word8 -> m ()
setZ v = store (P FZ) (v == 0)

-- Sets the negative flag
setN :: MonadEmulator m => Word8 -> m ()
setN v = store (P FN) (v .&. 0x80 /= 0)

-- Sets the overflow flag
setV :: MonadEmulator m => Word8 -> m ()
setV v = store (P FV) (v .&. 0x40 /= 0)

-- Sets the zero flag and the negative flag
setZN :: MonadEmulator m => Word8 -> m ()
setZN v = setZ v >> setN v

trace :: (MonadIO m, MonadEmulator m) => String -> m ()
trace v = liftIO $ putStrLn v

-- Fix this pos up
renderExecution :: MonadEmulator m => Opcode -> Word16 -> m String
renderExecution (Opcode raw mnem _) addr = do
  pcv <- load Pc
  spv <- load Sp
  av  <- load A
  xv  <- load X
  yv  <- load Y
  let (hiAddr, loAddr) = splitW16 addr
  pure $ (show mnem) ++ "  " ++
         (prettifyWord16 pcv) ++ "  " ++
         (prettifyWord8 raw) ++ " " ++
         (prettifyWord8 hiAddr) ++ " " ++
         (prettifyWord8 loAddr) ++ " " ++
         (replicate 33 ' ') ++
         "A:"  ++ (prettifyWord8 av) ++ " " ++
         "X:"  ++ (prettifyWord8 xv) ++ " " ++
         "Y:"  ++ (prettifyWord8 yv) ++ " " ++
        --  "P:"  ++ (prettifyWord8 pv) ++ " " ++
         "SP:" ++ (prettifyWord8 spv) ++ " "

