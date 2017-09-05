module Emulator (
  -- * Functions
    run
  , runDebug
  , r
  , execute
  , loadNextOpcode
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              (clearBit, setBit, testBit, xor, (.&.),
                                         (.|.))
import qualified Data.ByteString        as BS
import           Data.Word
import           Emulator.Cartridge
import           Emulator.Monad
import           Emulator.Nes           (Address (..), Flag (..))
import           Emulator.Opcode
import           Emulator.Trace         (Trace (..), renderTrace)
import           Emulator.Util
import           Prelude                hiding (and, compare)
import           Text.Printf            (printf)

r :: IO ()
r = void $ runDebug "roms/nestest.nes" (pure 0xC000)

run :: FilePath -> IO ()
run fp = do
  cart <- parseCartridge <$> BS.readFile fp
  runIOEmulator cart $ void emulate

runDebug :: FilePath -> Maybe Word16 -> IO [Trace]
runDebug fp startPc = do
  cart <- parseCartridge <$> BS.readFile fp
  runIOEmulator cart $ do
    case startPc of
      Just v  -> store Pc v
      Nothing -> pure ()
    emulateDebug

emulate :: MonadEmulator m => m ()
emulate = do
  opcode <- loadNextOpcode
  execute opcode
  emulate

emulateDebug :: (MonadIO m, MonadEmulator m) => m [Trace]
emulateDebug = go [] where
  go acc = do
    opcode <- loadNextOpcode
    trace <- execute opcode
    liftIO $ putStrLn $ renderTrace trace
    if (length acc) > 0 then
      pure acc
    else
      go (acc ++ [trace])

execute :: MonadEmulator m => Opcode -> m Trace
execute op @ (Opcode _ mn mode) = do
  addr <- addressForMode mode
  trace <- trace op addr
  incrementPc $ instructionLength op
  instructionMapping mn addr
  pure trace

loadNextOpcode :: MonadEmulator m => m Opcode
loadNextOpcode = do
  pc <- load Pc
  pcv <- load (Ram8 pc)
  pure $ decodeOpcode pcv

addressForMode :: MonadEmulator m => AddressMode -> m Word16
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

instructionMapping :: MonadEmulator m => Mnemonic -> (Word16 -> m ())
instructionMapping mnemonic = case mnemonic of
  AND     -> and
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
  CLD     -> const cld
  CLI     -> const cli
  CLV     -> const clv
  CMP     -> cmp
  CPX     -> cpx
  CPY     -> cpy
  EOR     -> eor
  INC     -> inc
  JMP     -> jmp
  JSR     -> jsr
  LDA     -> lda
  LDX     -> ldx
  NOP     -> const nop
  PHA     -> const pha
  PHP     -> const php
  PLA     -> const pla
  PLP     -> const plp
  ORA     -> ora
  RTS     -> const rts
  SEC     -> const sec
  SED     -> const sed
  SEI     -> const sei
  STA     -> sta
  STX     -> stx
  STY     -> sty
  TAX     -> tax
  TAY     -> tay
  TSX     -> tsx
  TXA     -> txa
  TXS     -> const txs
  TYA     -> const tya
  unknown -> error $ "Unimplemented opcode: " ++ (show unknown)

-- AND - Logical and
and :: MonadEmulator m => Word16 -> m ()
and addr = do
  av <- load A
  v <- load $ Ram8 addr
  store A (av .&. v)
  av' <- load A
  setZN av'

-- ADC - Add with carry
adc :: MonadEmulator m => Word16 -> m ()
adc addr = do
  av <- load A
  v <- load $ Ram8 addr
  cv <- getFlag Carry
  -- let newAv = av + v + cv
  -- store A newAv
  -- setZN newAv

  undefined


-- BCC - Branch on carry flag clear
bcc :: MonadEmulator m => Word16 -> m ()
bcc = branch $ not <$> getFlag Carry

-- BCS - Branch on carry flag set
bcs :: MonadEmulator m => Word16 -> m ()
bcs = branch $ getFlag Carry

-- BEQ - Branch if zero set
beq :: MonadEmulator m => Word16 -> m ()
beq = branch $ getFlag Zero

-- BMI - Branch if minus
bmi :: MonadEmulator m => Word16 -> m ()
bmi = branch $ getFlag Negative

-- BPL - Branch if positive
bpl :: MonadEmulator m => Word16 -> m ()
bpl = branch $ not <$> getFlag Negative

-- BVS - Branch if overflow clear
bvc :: MonadEmulator m => Word16 -> m ()
bvc = branch $ not <$> getFlag Overflow

-- BVS - Branch if overflow set
bvs :: MonadEmulator m => Word16 -> m ()
bvs = branch $ getFlag Overflow

-- BIT - Test Bits in memory with A
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
bne = branch $ not <$> getFlag Zero

-- CLC - Clear carry flag
clc :: MonadEmulator m => m ()
clc = setFlag Carry False

-- CLD - Clear decimal flag
cld :: MonadEmulator m => m ()
cld = setFlag Decimal False

-- CLI - Clear interrupt flag
cli :: MonadEmulator m => m ()
cli = setFlag Interrupt False

-- CLV - Clear overflow flag
clv :: MonadEmulator m => m ()
clv = setFlag Overflow False

-- CMP - Compare memory and A
cmp :: MonadEmulator m => Word16 -> m ()
cmp addr = do
  v <- load $ Ram8 addr
  av <- load A
  compare av v

-- CPX - Compare memory and X
cpx :: MonadEmulator m => Word16 -> m ()
cpx addr = do
  v <- load $ Ram8 addr
  xv <- load X
  compare xv v

-- CPY - Compare memory and Y
cpy :: MonadEmulator m => Word16 -> m ()
cpy addr = do
  v <- load $ Ram8 addr
  yv <- load Y
  compare yv v

-- EOR - Exclusive or
eor :: MonadEmulator m => Word16 -> m ()
eor addr = do
  v <- load $ Ram8 addr
  av <- load A
  let newAv = av `xor` v
  store A newAv
  setZN av


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
jsr :: MonadEmulator m => Word16 -> m ()
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

-- PHP - Push processor status onto stack
php :: MonadEmulator m => m ()
php = do
  p <- load P
  push $ p .|. 0x10

-- PLA - Pull Accumulator register
pla :: MonadEmulator m => m ()
pla = do
  v <- pull
  store A v
  setZN v

-- PLP - Pull Accumulator register
plp :: MonadEmulator m => m ()
plp = do
  v <- pull
  store P ((v .&. 0xEF) .|. 0x20)

-- PHA - Push Accumulator register
pha :: MonadEmulator m => m ()
pha = do
  av <- load A
  push av

-- RTS - Return from a subroutine
rts :: MonadEmulator m => m ()
rts = do
  addr <- pull16
  store Pc (addr + 1)

-- ORA - Logical Inclusive OR
ora :: MonadEmulator m => Word16 -> m ()
ora addr = do
  v <- load$ Ram8 addr
  av <- load A
  let newAv = av .|. v
  store A av
  setZN newAv

-- SEC - Set carry flag
sec :: MonadEmulator m => m ()
sec = setFlag Carry True

-- SED - Set decimal flag
sed :: MonadEmulator m => m ()
sed = setFlag Decimal True

-- SEI - Set interrupt flag
sei :: MonadEmulator m => m ()
sei = setFlag Interrupt True

-- STA - Store Accumulator register
sta :: MonadEmulator m => Word16 -> m ()
sta addr = (load A) >>= (store $ Ram8 addr)

-- STX - Store X register
stx :: MonadEmulator m => Word16 -> m ()
stx addr = (load X) >>= (store $ Ram8 addr)

-- STY - Store Y register
sty :: MonadEmulator m => Word16 -> m ()
sty addr = (load Y) >>= (store $ Ram8 addr)

-- TAX - Transfer Accumulator to X
tax :: MonadEmulator m => Word16 -> m ()
tax addr = do
  av <- load A
  store X av
  xv <- load X
  setZN xv

-- TAY - Transfer Accumulator to Y
tay :: MonadEmulator m => Word16 -> m ()
tay addr =  do
  av <- load A
  store Y av
  yv <- load Y
  setZN yv

-- TSX - Transfer Stack Pointer to X
tsx :: MonadEmulator m => Word16 -> m ()
tsx addr = do
  spv <- load Sp
  store X spv
  xv <- load X
  setZN xv

-- TXA - Transfer X to Accumulator
txa :: MonadEmulator m => Word16 -> m ()
txa addr = do
  xv <- load X
  store A xv
  av <- load A
  setZN av

-- TXS - Transfer X to Stack Pointer
txs :: MonadEmulator m => m ()
txs = do
  xv <- load X
  store Sp xv

-- TYA - Transfer Y to Accumulator
tya :: MonadEmulator m => m ()
tya = do
  yv <- load Y
  store A yv
  av <- load A
  setZN av

incrementPc :: MonadEmulator m => Word16 -> m ()
incrementPc n = do
  pc <- load Pc
  store Pc (pc + n)

-- Moves execution to addr if condition is set
branch :: MonadEmulator m => (m Bool) -> Word16 -> m ()
branch cond addr = do
  c <- cond
  if c then
    store Pc addr
  else
    pure ()

getFlag :: MonadEmulator m => Flag -> m Bool
getFlag flag = do
  v <- load P
  pure $ testBit v (7 - fromEnum flag)

setFlag :: MonadEmulator m => Flag -> Bool -> m ()
setFlag flag b = do
  v <- load P
  store P (opBit v (7 - fromEnum flag))
  where opBit = if b then setBit else clearBit

pull :: MonadEmulator m => m Word8
pull = do
  spv <- load Sp
  store Sp (spv + 1)
  let i = 0x100 .|. (toWord16 spv + 1)
  load $ Ram8 i

pull16 :: MonadEmulator m => m Word16
pull16 = do
  lo <- pull
  hi <- pull
  pure $ makeW16 lo hi

push :: MonadEmulator m => Word8 -> m ()
push v = do
  spv <- load Sp
  let i = 0x100 .|. (toWord16 spv)
  store (Ram8 i) v
  store Sp (spv - 1)

push16 :: MonadEmulator m => Word16 -> m ()
push16 v = do
  let (lo, hi) = splitW16 v
  push hi
  push lo

-- Sets the zero flag
setZ :: MonadEmulator m => Word8 -> m ()
setZ v = setFlag Zero (v == 0)

-- Sets the negative flag
setN :: MonadEmulator m => Word8 -> m ()
setN v = setFlag Negative (v .&. 0x80 /= 0)

-- Sets the overflow flag
setV :: MonadEmulator m => Word8 -> m ()
setV v = setFlag Overflow (v .&. 0x40 /= 0)

-- Sets the zero flag and the negative flag
setZN :: MonadEmulator m => Word8 -> m ()
setZN v = setZ v >> setN v

compare :: MonadEmulator m => Word8 -> Word8 -> m ()
compare a b = do
  setZN $ a - b
  if a >= b then
    setFlag Carry True
  else
    setFlag Carry False

trace :: MonadEmulator m => Opcode -> Word16 -> m Trace
trace op addr = do
  pcv <- load Pc
  a0 <- load (Ram8 $ pcv)
  a1 <- load (Ram8 $ pcv + 1)
  a2 <- load (Ram8 $ pcv + 2)
  spv <- load Sp
  av  <- load A
  xv  <- load X
  yv  <- load Y
  pv  <- load P
  pure (Trace pcv spv av xv yv pv op a0 a1 a2)
