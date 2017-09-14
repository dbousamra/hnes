module Emulator (
    run
  , runDebug
  , r
  , execute
  , loadNextOpcode
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              hiding (bit)
import qualified Data.ByteString        as BS
import           Data.Word
import           Emulator.Address
import           Emulator.Cartridge
import           Emulator.Monad
import           Emulator.Opcode
import           Emulator.Trace         (Trace (..), renderTrace)
import           Emulator.Util
import           Prelude                hiding (and, compare)
import           Text.Printf            (printf)

r :: IO ()
r = void $ runDebug "roms/color_test.nes" Nothing

run :: FilePath -> IO ()
run fp = void $ runDebug fp Nothing

runDebug :: FilePath -> Maybe Word16 -> IO [Trace]
runDebug fp startPc = do
  cart <- parseCartridge <$> BS.readFile fp
  runIOEmulator cart $ do
    case startPc of
      Just v  -> store Pc v
      Nothing -> reset
    emulateDebug

emulate :: (MonadIO m, MonadEmulator m) => m ()
emulate = do
  opcode <- loadNextOpcode
  execute opcode
  emulate

emulateDebug :: (MonadIO m, MonadEmulator m) => m [Trace]
emulateDebug = go [] where
  go acc = do
    pcv <- load Pc
    opcode <- loadNextOpcode
    trace <- execute opcode
    liftIO $ putStrLn $ renderTrace trace
    go (acc ++ [trace])

execute :: (MonadIO m, MonadEmulator m) => Opcode -> m Trace
execute op @ (Opcode _ _ mode) = do
  addr <- addressForMode mode
  trace <- trace op addr
  incrementPc $ instructionLength op
  runInstruction op addr
  pure trace

reset :: MonadEmulator m => m ()
reset = do
  v <- load (Ram16 0xFFFC)
  store Pc v
  store Sp 0xFD
  store P 0x24

loadNextOpcode :: MonadEmulator m => m Opcode
loadNextOpcode = do
  pc <- load Pc
  pcv <- load (Ram8 pc)
  pure $ decodeOpcode pcv

addressForMode :: (MonadIO m, MonadEmulator m) => AddressMode -> m Word16
addressForMode mode = case mode of
  Absolute -> do
    pcv <- load Pc
    load $ Ram16 (pcv + 1)
  AbsoluteX -> do
    pcv <- load Pc
    xv <- load X
    v <- load $ Ram16 (pcv + 1)
    pure $ v + toWord16 xv
  AbsoluteY -> do
    pcv <- load Pc
    yv <- load Y
    v <- load $ Ram16 (pcv + 1)
    pure $ v + toWord16 yv
  Accumulator ->
    pure 0
  Immediate -> do
    pcv <- load Pc
    pure $ pcv + 1
  Implied ->
    pure 0
  Indirect -> do
    pcv <- load Pc
    addr <- load $ Ram16 (pcv + 1)
    yo <- read16Bug addr
    pure yo
  IndirectIndexed -> do
    pcv <- load Pc
    yv <- load Y
    v <- load (Ram8 $ pcv + 1)
    addr <- read16Bug $ toWord16 v
    pure $ addr + toWord16 yv
  IndexedIndirect -> do
    pcv <- load Pc
    xv <- load X
    v <- load (Ram8 $ pcv + 1)
    read16Bug $ toWord16 (v + xv)
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
  ZeroPageX -> do
    pcv <- load Pc
    xv <- load X
    v <- load $ Ram8 (pcv + 1)
    pure $ toWord16 (v + xv)
  ZeroPageY -> do
    pcv <- load Pc
    yv <- load Y
    v <- load $ Ram8 (pcv + 1)
    pure $ toWord16 (v + yv)

runInstruction :: (MonadIO m, MonadEmulator m) => Opcode -> (Word16 -> m ())
runInstruction (Opcode _ mnemonic mode) = case mnemonic of
  ADC -> adc
  AND -> and
  ASL -> asl mode
  BCC -> bcc
  BCS -> bcs
  BEQ -> beq
  BIT -> bit
  BMI -> bmi
  BNE -> bne
  BPL -> bpl
  BRK -> brk
  BVC -> bvc
  BVS -> bvs
  CLC -> const clc
  CLD -> const cld
  CLI -> const cli
  CLV -> const clv
  CMP -> cmp
  CPX -> cpx
  CPY -> cpy
  DEC -> dec
  DEX -> const dex
  DEY -> const dey
  EOR -> eor
  INC -> inc
  INX -> const inx
  INY -> const iny
  JMP -> jmp
  JSR -> jsr
  LDA -> lda
  LDX -> ldx
  LDY -> ldy
  LSR -> lsr mode
  NOP -> const nop
  PHA -> const pha
  PHP -> const php
  PLA -> const pla
  PLP -> const plp
  ORA -> ora
  RTI -> const rti
  RTS -> const rts
  ROR -> ror mode
  ROL -> rol mode
  SBC -> sbc
  SEC -> const sec
  SED -> const sed
  SEI -> const sei
  STA -> sta
  STX -> stx
  STY -> sty
  TAX -> tax
  TAY -> tay
  TSX -> tsx
  TXA -> txa
  TXS -> const txs
  TYA -> const tya
  KIL -> const $ illegal mnemonic
  LAX -> lax
  SAX -> sax
  DCP -> dcp
  ISC -> isc
  RLA -> rla mode
  RRA -> rra mode
  SLO -> slo mode
  SRE -> sre mode
  ANC -> const $ illegal mnemonic
  ALR -> const $ illegal mnemonic
  ARR -> const $ illegal mnemonic
  XAA -> const $ illegal mnemonic
  AHX -> const $ illegal mnemonic
  TAS -> const $ illegal mnemonic
  SHX -> const $ illegal mnemonic
  SHY -> const $ illegal mnemonic
  LAS -> const $ illegal mnemonic
  AXS -> const $ illegal mnemonic

-- Official instructions

-- ADC - Add with carry
adc :: MonadEmulator m => Word16 -> m ()
adc addr = do
  av <- load A
  bv <- load $ Ram8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  store A (av + bv + cv)
  av' <- load A
  setZN av'
  let shouldCarry = toInt av + toInt bv + toInt cv > 0xFF
  let doesOverflow = ((av `xor` bv) .&. 0x80) == 0 && ((av `xor` av') .&. 0x80) /= 0
  setFlag Carry shouldCarry
  setFlag Overflow doesOverflow

-- ASL - Arithmetic shift left
asl :: MonadEmulator m => AddressMode -> Word16 -> m ()
asl mode addr = do
  v <- load dest
  let i = (v `shiftR` 7) .&. 1
  setFlag Carry (toEnum . fromIntegral $ i)
  let shiftedV = v `shiftL` 1
  store dest shiftedV
  setZN shiftedV
  where
    dest = case mode of
      Accumulator -> A
      other       -> Ram8 addr

-- AND - Logical and
and :: MonadEmulator m => Word16 -> m ()
and addr = do
  av <- load A
  v <- load $ Ram8 addr
  store A (av .&. v)
  av' <- load A
  setZN av'

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

-- BRK - Force interrupt
brk :: MonadEmulator m => Word16 -> m ()
brk addr = do
  pcv <- load Pc
  push16 pcv
  php
  sei
  av <- load (Ram16 0xFFFE)
  store Pc av

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

-- DEC - Decrement memory
dec :: MonadEmulator m => Word16 -> m ()
dec addr = do
  v <- load $ Ram8 addr
  let value = v - 1
  store (Ram8 addr) value
  setZN value

-- DEX - Decrement X register
dex :: MonadEmulator m => m ()
dex = do
  v <- load X
  let value = v - 1
  store X value
  setZN value


-- DEY - Decrement Y register
dey :: MonadEmulator m => m ()
dey = do
  v <- load Y
  let value = v - 1
  store Y value
  setZN value

-- EOR - Exclusive or
eor :: MonadEmulator m => Word16 -> m ()
eor addr = do
  v <- load $ Ram8 addr
  av <- load A
  let newAv = av `xor` v
  store A newAv
  setZN newAv


-- INC - Increment memory
inc :: MonadEmulator m => Word16 -> m ()
inc addr = do
  v <- load $ Ram8 addr
  let value = v + 1
  store (Ram8 addr) value
  setZN value

-- INX - Increment X register
inx :: MonadEmulator m => m ()
inx  = do
  v <- load X
  let value = v + 1
  store X value
  setZN value

-- INY - Increment Y register
iny :: MonadEmulator m => m ()
iny  = do
  v <- load Y
  let value = v + 1
  store Y value
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
lda :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
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

-- LDY - Load Y Register
ldy :: MonadEmulator m => Word16 -> m ()
ldy addr = do
  v <- load $ Ram8 addr
  store Y v
  setZN v

-- LSR - Logical shift right
lsr :: MonadEmulator m => AddressMode -> Word16 -> m ()
lsr mode addr = do
  v <- load dest
  setFlag Carry (toEnum . fromIntegral $ v .&. 1)
  let shiftedV = v `shiftR` 1
  store dest shiftedV
  setZN shiftedV
  where
    dest = case mode of
      Accumulator -> A
      other       -> Ram8 addr

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

-- ORA - Logical Inclusive OR
ora :: MonadEmulator m => Word16 -> m ()
ora addr = do
  v <- load$ Ram8 addr
  av <- load A
  let newAv = av .|. v
  store A newAv
  setZN newAv

-- ROL - Rotate left
rol :: MonadEmulator m => AddressMode -> Word16 -> m ()
rol mode addr = do
  v <- load dest
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  setFlag Carry (toEnum $ fromIntegral $ (v `shiftR` 7) .&. 1)
  let shiftedV = (v `shiftL` 1) .|. cv
  store dest shiftedV
  setZN shiftedV
  where
    dest = case mode of
      Accumulator -> A
      other       -> Ram8 addr

-- ROR - Rotate right
ror :: MonadEmulator m => AddressMode -> Word16 -> m ()
ror mode addr = do
  v <- load dest
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  setFlag Carry (toEnum $ fromIntegral $ v .&. 1)
  let shiftedV = (v `shiftR` 1) .|. (cv `shiftL` 7)
  store dest shiftedV
  setZN shiftedV
  where
    dest = case mode of
      Accumulator -> A
      other       -> Ram8 addr

-- RTI - Return from interrupt
rti :: MonadEmulator m => m ()
rti = do
  addr <- pull
  store P (addr .&. 0xEf .|. 0x20)
  addr' <- pull16
  store Pc addr'

-- RTS - Return from a subroutine
rts :: MonadEmulator m => m ()
rts = do
  addr <- pull16
  store Pc (addr + 1)

-- SBC - Subtract with carry
sbc :: MonadEmulator m => Word16 -> m ()
sbc addr = do
  av <- load A
  bv <- load $ Ram8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  store A (av - bv - (1 - cv))
  av' <- load A
  setZN av'
  let shouldCarry = toInt av - toInt bv - toInt (1 - cv) >= 0
  let doesOverflow = ((av `xor` bv) .&. 0x80) /= 0 && ((av `xor` av') .&. 0x80) /= 0
  setFlag Carry shouldCarry
  setFlag Overflow doesOverflow

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
sta :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
sta addr = do
  av <- load A
  store (Ram8 addr) av

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

-- Illegal instructions:

-- LAX - Load Accumulator and X with memory
lax :: MonadEmulator m => Word16 -> m ()
lax addr = do
  v <- load (Ram8 addr)
  store A v
  store X v
  setZN v

-- SAX - AND X register with Accumulator and store result in memory
sax :: MonadEmulator m => Word16 -> m ()
sax addr = do
  av <- load A
  xv <- load X
  store (Ram8 addr) (av .&. xv)

-- DCP - Subtract 1 from memory
dcp :: MonadEmulator m => Word16 -> m ()
dcp addr = dec addr >> cmp addr

-- ISC - INCs the contents of a memory location and then SBCs the result
-- from the A register.
isc :: MonadEmulator m => Word16 -> m ()
isc addr = inc addr >> sbc addr

-- RLA - ROLs the contents of a memory location and then ANDs the result with
-- the Accumulator.
rla :: MonadEmulator m => AddressMode -> Word16 -> m ()
rla mode addr = rol mode addr >> and addr

-- SLO - ASLs the contents of a memory location and then ORs the result
-- with the Accumulator.
slo :: MonadEmulator m => AddressMode -> Word16 -> m ()
slo mode addr = asl mode addr >> ora addr

-- SRE - LSRs the contents of a memory location and then EORs the result with
-- the Accumulator.
sre :: MonadEmulator m => AddressMode -> Word16 -> m ()
sre mode addr = lsr mode addr >> eor addr

-- RRA - RORs the contents of a memory location and then ADCs the result with
-- the Accumulator
rra :: MonadEmulator m => AddressMode -> Word16 -> m ()
rra mode addr = ror mode addr >> adc addr

incrementPc :: MonadEmulator m => Word16 -> m ()
incrementPc n = do
  pc <- load Pc
  store Pc (pc + n)

-- Moves execution to addr if condition is set
branch :: MonadEmulator m => m Bool -> Word16 -> m ()
branch cond addr = do
  c <- cond
  if c then
    store Pc addr
  else
    pure ()

read16Bug :: MonadEmulator m => Word16 -> m Word16
read16Bug addr = do
  lo <- load $ Ram8 addr
  hi <- load $ Ram8 $ (addr .&. 0xFF00) .|. (toWord16 $ (toWord8 addr) + 1)
  pure $ makeW16 lo hi

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
  setFlag Carry (a >= b)

illegal :: (MonadIO m, MonadEmulator m) => Mnemonic -> m ()
illegal mnemonic = liftIO $ putStrLn $ "illegal opcode" ++ (show mnemonic)

trace :: (MonadIO m, MonadEmulator m) => Opcode -> Word16 -> m Trace
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
  let instrLength = instructionLength op
      a1R = if instrLength < 2 then 0x0 else a1
      a2R = if instrLength < 3 then 0x0 else a2
  pure (Trace pcv spv av xv yv pv op a0 a1R a2R)
