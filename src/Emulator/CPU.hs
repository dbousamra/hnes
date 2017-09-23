module Emulator.CPU(
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              hiding (bit)
import           Data.Word
import           Emulator.Monad
import           Emulator.Nes           hiding (cycles)
import           Emulator.Opcode
import           Emulator.Trace         (Trace (..), renderTrace)
import           Emulator.Util
import           Prelude                hiding (and, compare, cycles)

reset :: MonadEmulator m => m ()
reset = do
  v <- load (Cpu $ CpuMemory16 0xFFFC)
  store (Cpu Pc) v
  store (Cpu Sp) 0xFD
  store (Cpu P) 0x24

step :: (MonadIO m, MonadEmulator m) => m (Int, Trace)
step = do
  startingCycles <- load $ Cpu CpuCycles
  opcode <- loadNextOpcode
  (pageCrossed, addr) <- addressPageCrossForMode (mode opcode)
  trace <- trace opcode addr
  addCycles $ getCycles opcode pageCrossed
  incrementPc opcode
  runInstruction opcode addr
  endingCycles <- load $ Cpu CpuCycles
  pure (endingCycles - startingCycles, trace)

trace :: MonadEmulator m => Opcode -> Word16 -> m Trace
trace op addr = do
  pcv <- load $ Cpu Pc
  a0 <- load $ Cpu $ CpuMemory8 pcv
  a1 <- load $ Cpu $ CpuMemory8 (pcv + 1)
  a2 <- load $ Cpu $ CpuMemory8 (pcv + 2)
  spv <- load $ Cpu Sp
  av  <- load $ Cpu A
  xv  <- load $ Cpu X
  yv  <- load $ Cpu Y
  pv  <- load $ Cpu P
  cycles <- load $ Cpu CpuCycles
  let instrLength = len op
  let a1R = if instrLength < 2 then 0x0 else a1
  let a2R = if instrLength < 3 then 0x0 else a2
  pure (Trace pcv spv av xv yv pv op a0 a1R a2R ((cycles * 3) `mod` 341))

loadNextOpcode :: MonadEmulator m => m Opcode
loadNextOpcode = do
  pcv <- load $ Cpu Pc
  av <- load (Cpu $ CpuMemory8 pcv)
  pure $ decodeOpcode av

addressPageCrossForMode :: MonadEmulator m => AddressMode -> m (Bool, Word16)
addressPageCrossForMode mode = case mode of
  Absolute -> do
    pcv <- load $ Cpu Pc
    addrV <- load $ Cpu $ CpuMemory16 (pcv + 1)
    pure (False, addrV)
  AbsoluteX -> do
    pcv <- load $ Cpu Pc
    xv <- load $ Cpu X
    v <- load $ Cpu $ CpuMemory16 (pcv + 1)
    let addrV = v + toWord16 xv
    let pageCrossed = differentPages (addrV - (toWord16 xv)) addrV
    pure (pageCrossed, addrV)
  AbsoluteY -> do
    pcv <- load $ Cpu Pc
    yv <- load $ Cpu Y
    v <- load $ Cpu $ CpuMemory16 (pcv + 1)
    let addrV = v + toWord16 yv
    let pageCrossed = differentPages (addrV - (toWord16 yv)) addrV
    pure (pageCrossed, addrV)
  Accumulator ->
    pure (False, 0)
  Immediate -> do
    pcv <- load $ Cpu Pc
    pure (False, pcv + 1)
  Implied ->
    pure (False, 0)
  Indirect -> do
    pcv <- load $ Cpu Pc
    addr <- load $ Cpu $ CpuMemory16 (pcv + 1)
    yo <- read16Bug addr
    pure (False, yo)
  IndirectIndexed -> do
    pcv <- load $ Cpu Pc
    yv <- load $ Cpu Y
    v <- load (Cpu $ CpuMemory8 $ pcv + 1)
    addr <- read16Bug $ toWord16 v
    let addrV = addr + toWord16 yv
    let pageCrossed = differentPages (addrV - (toWord16 yv)) addrV
    pure (pageCrossed, addrV)
  IndexedIndirect -> do
    pcv <- load $ Cpu Pc
    xv <- load $ Cpu X
    v <- load $ Cpu $ CpuMemory8 $ pcv + 1
    addrV <- read16Bug $ toWord16 (v + xv)
    pure (False, addrV)
  Relative -> do
    pcv <- load $ Cpu Pc
    offset16 <- load $ Cpu $ CpuMemory16 (pcv + 1)
    let offset8 = firstNibble offset16
    if offset8 < 0x80 then
      pure (False, pcv + 2 + offset8)
    else
      pure (False, pcv + 2 + offset8 - 0x100)
  ZeroPage -> do
    pcv <- load $ Cpu Pc
    v <- load $ Cpu $ CpuMemory8 (pcv + 1)
    pure (False, toWord16 v)
  ZeroPageX -> do
    pcv <- load $ Cpu Pc
    xv <- load $ Cpu X
    v <- load $ Cpu $ CpuMemory8 (pcv + 1)
    pure (False, toWord16 $ v + xv)
  ZeroPageY -> do
    pcv <- load $ Cpu Pc
    yv <- load $ Cpu Y
    v <- load $ Cpu $ CpuMemory8 (pcv + 1)
    pure (False, toWord16 $ v + yv)

differentPages :: Word16 -> Word16 -> Bool
differentPages a b = (a .&. 0xFF00) /= (b .&. 0xFF00)

incrementPc :: MonadEmulator m => Opcode -> m ()
incrementPc opcode = modify (Cpu Pc) (+ instrLength)
  where instrLength = fromIntegral $ (len opcode)

getCycles :: Opcode -> Bool -> Int
getCycles opcode pageCrossed = if pageCrossed
  then pageCrossCycles opcode + cycles opcode
  else cycles opcode

modify :: MonadEmulator m => Address a -> (a -> a) -> m ()
modify addr f = do
  av <- load addr
  store addr (f av)

runInstruction :: (MonadIO m, MonadEmulator m) => Opcode -> (Word16 -> m ())
runInstruction (Opcode _ mnemonic mode _ _ _) = case mnemonic of
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
  av <- load $ Cpu A
  bv <- load $ Cpu $ CpuMemory8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  store (Cpu A) (av + bv + cv)
  av' <- load $ Cpu A
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
      Accumulator -> Cpu A
      other       -> Cpu $ CpuMemory8 addr

-- AND - Logical and
and :: MonadEmulator m => Word16 -> m ()
and addr = do
  av <- load $ Cpu A
  v <- load $ Cpu $ CpuMemory8 addr
  store (Cpu A) (av .&. v)
  av' <- load $ Cpu A
  setZN av'

-- BCC - Branch on carry flag clear
bcc :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
bcc = branch $ not <$> getFlag Carry

-- BCS - Branch on carry flag set
bcs :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
bcs = branch $ getFlag Carry

-- BEQ - Branch if zero set
beq :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
beq = branch $ getFlag Zero

-- BMI - Branch if minus
bmi :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
bmi = branch $ getFlag Negative

-- BPL - Branch if positive
bpl :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
bpl = branch $ not <$> getFlag Negative

-- BVS - Branch if overflow clear
bvc :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
bvc = branch $ not <$> getFlag Overflow

-- BVS - Branch if overflow set
bvs :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
bvs = branch $ getFlag Overflow

-- BRK - Force interrupt
brk :: MonadEmulator m => Word16 -> m ()
brk addr = do
  pcv <- load $ Cpu Pc
  push16 pcv
  php
  sei
  av <- load (Cpu $ CpuMemory16 0xFFFE)
  store (Cpu Pc) av

-- BIT - Test Bits in memory with A
bit :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
bit addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  av <- load $ Cpu A
  let res = (v .&. av)
  setZ res
  setV v
  setN v

-- BNE - Branch if zero not set
bne :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
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
  v <- load $ Cpu $ CpuMemory8 addr
  av <- load $ Cpu A
  compare av v

-- CPX - Compare memory and X
cpx :: MonadEmulator m => Word16 -> m ()
cpx addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  xv <- load $ Cpu X
  compare xv v

-- CPY - Compare memory and Y
cpy :: MonadEmulator m => Word16 -> m ()
cpy addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  yv <- load $ Cpu Y
  compare yv v

-- DEC - Decrement memory
dec :: MonadEmulator m => Word16 -> m ()
dec addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  let value = v - 1
  store (Cpu $ CpuMemory8 addr) value
  setZN value

-- DEX - Decrement X register
dex :: MonadEmulator m => m ()
dex = do
  v <- load $ Cpu X
  let value = v - 1
  store (Cpu X) value
  setZN value


-- DEY - Decrement Y register
dey :: MonadEmulator m => m ()
dey = do
  v <- load $ Cpu Y
  let value = v - 1
  store (Cpu Y) value
  setZN value

-- EOR - Exclusive or
eor :: MonadEmulator m => Word16 -> m ()
eor addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  av <- load $ Cpu A
  let newAv = av `xor` v
  store (Cpu A) newAv
  setZN newAv


-- INC - Increment memory
inc :: MonadEmulator m => Word16 -> m ()
inc addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  let value = v + 1
  store (Cpu $ CpuMemory8 addr) value
  setZN value

-- INX - Increment X register
inx :: MonadEmulator m => m ()
inx  = do
  v <- load $ Cpu X
  let value = v + 1
  store (Cpu X) value
  setZN value

-- INY - Increment Y register
iny :: MonadEmulator m => m ()
iny  = do
  v <- load $ Cpu Y
  let value = v + 1
  store (Cpu Y) value
  setZN value

-- JMP - Move execution to a particular address
jmp :: MonadEmulator m => Word16 -> m ()
jmp = store $ Cpu Pc

-- JSR - Jump to subroutine
jsr :: MonadEmulator m => Word16 -> m ()
jsr addr = do
  pcv <- load $ Cpu Pc
  push16 $ pcv - 1
  store (Cpu Pc) addr

-- LDA - Load accumulator register
lda :: (MonadIO m, MonadEmulator m) => Word16 -> m ()
lda addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  store (Cpu A) v
  av <- load $ Cpu A
  setZN v

-- LDX - Load X Register
ldx :: MonadEmulator m => Word16 -> m ()
ldx addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  store (Cpu X) v
  setZN v

-- LDY - Load Y Register
ldy :: MonadEmulator m => Word16 -> m ()
ldy addr = do
  v <- load $ Cpu $ CpuMemory8 addr
  store (Cpu Y) v
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
      Accumulator -> Cpu A
      other       -> Cpu $ CpuMemory8 addr

-- NOP - No operation. Do nothing :D
nop :: MonadEmulator m => m ()
nop = pure ()

-- PHP - Push processor status onto stack
php :: MonadEmulator m => m ()
php = do
  p <- load $ Cpu P
  push $ p .|. 0x10

-- PLA - Pull Accumulator register
pla :: MonadEmulator m => m ()
pla = do
  v <- pull
  store (Cpu A) v
  setZN v

-- PLP - Pull Accumulator register
plp :: MonadEmulator m => m ()
plp = do
  v <- pull
  store (Cpu P) ((v .&. 0xEF) .|. 0x20)

-- PHA - Push Accumulator register
pha :: MonadEmulator m => m ()
pha = do
  av <- load $ Cpu A
  push av

-- ORA - Logical Inclusive OR
ora :: MonadEmulator m => Word16 -> m ()
ora addr = do
  v <- load$ Cpu $ CpuMemory8 addr
  av <- load $ Cpu A
  let newAv = av .|. v
  store (Cpu A) newAv
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
      Accumulator -> Cpu A
      other       -> Cpu $ CpuMemory8 addr

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
      Accumulator -> Cpu A
      other       -> Cpu $ CpuMemory8 addr

-- RTI - Return from interrupt
rti :: MonadEmulator m => m ()
rti = do
  addr <- pull
  store (Cpu P) (addr .&. 0xEf .|. 0x20)
  addr' <- pull16
  store (Cpu Pc) addr'

-- RTS - Return from a subroutine
rts :: MonadEmulator m => m ()
rts = do
  addr <- pull16
  store (Cpu Pc) (addr + 1)

-- SBC - Subtract with carry
sbc :: MonadEmulator m => Word16 -> m ()
sbc addr = do
  av <- load $ Cpu A
  bv <- load $ Cpu $ CpuMemory8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  store (Cpu A) (av - bv - (1 - cv))
  av' <- load $ Cpu A
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
sta :: MonadEmulator m => Word16 -> m ()
sta addr = do
  av <- load $ Cpu A
  store (Cpu $ CpuMemory8 addr) av

-- STX - Store X register
stx :: MonadEmulator m => Word16 -> m ()
stx addr = (load $ Cpu X) >>= (store $ Cpu $ CpuMemory8 addr)

-- STY - Store Y register
sty :: MonadEmulator m => Word16 -> m ()
sty addr = (load $ Cpu Y) >>= (store $ Cpu $ CpuMemory8 addr)

-- TAX - Transfer Accumulator to X
tax :: MonadEmulator m => Word16 -> m ()
tax addr = do
  av <- load $ Cpu A
  store (Cpu X) av
  xv <- load $ Cpu X
  setZN xv

-- TAY - Transfer Accumulator to Y
tay :: MonadEmulator m => Word16 -> m ()
tay addr =  do
  av <- load $ Cpu A
  store (Cpu Y) av
  yv <- load $ Cpu Y
  setZN yv

-- TSX - Transfer Stack Pointer to X
tsx :: MonadEmulator m => Word16 -> m ()
tsx addr = do
  spv <- load $ Cpu Sp
  store (Cpu X) spv
  xv <- load $ Cpu X
  setZN xv

-- TXA - Transfer X to Accumulator
txa :: MonadEmulator m => Word16 -> m ()
txa addr = do
  xv <- load $ Cpu X
  store (Cpu A) xv
  av <- load $ Cpu A
  setZN av

-- TXS - Transfer X to Stack Pointer
txs :: MonadEmulator m => m ()
txs = do
  xv <- load $ Cpu X
  store (Cpu Sp) xv

-- TYA - Transfer Y to Accumulator
tya :: MonadEmulator m => m ()
tya = do
  yv <- load $ Cpu Y
  store (Cpu A) yv
  av <- load $ Cpu A
  setZN av

-- Illegal instructions:

-- LAX - Load Accumulator and X with memory
lax :: MonadEmulator m => Word16 -> m ()
lax addr = do
  v <- load (Cpu $ CpuMemory8 addr)
  store (Cpu A) v
  store (Cpu X) v
  setZN v

-- SAX - AND X register with Accumulator and store result in memory
sax :: MonadEmulator m => Word16 -> m ()
sax addr = do
  av <- load $ Cpu A
  xv <- load $ Cpu X
  store (Cpu $ CpuMemory8 addr) (av .&. xv)

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

-- Moves execution to addr if condition is set
branch :: (MonadIO m, MonadEmulator m) => m Bool -> Word16 -> m ()
branch cond addr = do
  cv <- cond
  when cv $ do
    store (Cpu Pc) addr
    let cycles = if differentPages addr addr then 2 else 1
    addCycles cycles

read16Bug :: MonadEmulator m => Word16 -> m Word16
read16Bug addr = do
  lo <- load $ Cpu $ CpuMemory8 addr
  hi <- load $ Cpu $ CpuMemory8 $ (addr .&. 0xFF00) .|. (toWord16 $ (toWord8 addr) + 1)
  pure $ makeW16 lo hi

getFlag :: MonadEmulator m => Flag -> m Bool
getFlag flag = do
  v <- load $ Cpu P
  pure $ testBit v (7 - fromEnum flag)

setFlag :: MonadEmulator m => Flag -> Bool -> m ()
setFlag flag b = do
  v <- load $ Cpu P
  store (Cpu P) (opBit v (7 - fromEnum flag))
  where opBit = if b then setBit else clearBit

pull :: MonadEmulator m => m Word8
pull = do
  spv <- load $ Cpu Sp
  store (Cpu Sp) (spv + 1)
  let i = 0x100 .|. (toWord16 spv + 1)
  load $ Cpu $ CpuMemory8 i

pull16 :: MonadEmulator m => m Word16
pull16 = do
  lo <- pull
  hi <- pull
  pure $ makeW16 lo hi

push :: MonadEmulator m => Word8 -> m ()
push v = do
  spv <- load $ Cpu Sp
  let i = 0x100 .|. (toWord16 spv)
  store (Cpu $ CpuMemory8 i) v
  store (Cpu Sp) (spv - 1)

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

illegal :: MonadEmulator m => Mnemonic -> m ()
illegal mnemonic = pure ()

addCycles :: MonadEmulator m => Int -> m ()
addCycles n = modify (Cpu CpuCycles) (+ n)
