module Emulator.CPU(
    reset
  , step
  , stepT
) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Bits              hiding (bit)
import           Data.Word
import           Emulator.Nes
import           Emulator.Opcode
import           Emulator.Trace         (Trace, mkTrace)
import           Emulator.Util
import           Prelude                hiding (and, compare)

reset :: Emulator ()
reset = do
  v <- readCpuMemory16 0xFFFC
  storeCpu pc v
  storeCpu sp 0xFD
  storeCpu p 0x24

step :: Emulator Int
step = do
  -- Start counting the number of cycles.
  -- Some of the opcodes (the branch ones)
  -- modify the cycle count directly due to page crosses
  startingCycles <- loadCpu cpuCycles
  handleInterrupts
  opcode <- loadNextOpcode
  (pageCrossed, addr) <- addressPageCrossForMode (mode opcode)
  incrementPc opcode
  addCycles $ getCycles opcode pageCrossed
  runInstruction opcode addr
  endingCycles <- loadCpu cpuCycles
  pure $ endingCycles - startingCycles

stepT :: Emulator (Int, Trace)
stepT = do

  -- Start counting the number of cycles.
  -- Some of the opcodes (the branch ones)
  -- modify the cycle count directly due to page crosses
  startingCycles <- loadCpu cpuCycles
  handleInterrupts
  opcode <- loadNextOpcode
  trace <- mkTrace opcode
  (pageCrossed, addr) <- addressPageCrossForMode (mode opcode)
  incrementPc opcode
  addCycles $ getCycles opcode pageCrossed
  runInstruction opcode addr
  endingCycles <- loadCpu cpuCycles
  pure (endingCycles - startingCycles, trace)

loadNextOpcode :: Emulator Opcode
loadNextOpcode = do
  pcv <- loadCpu pc
  av <- readCpuMemory8 pcv
  pure $ decodeOpcode av

addressPageCrossForMode :: AddressMode -> Emulator (Bool, Word16)
addressPageCrossForMode mode = case mode of
  Absolute -> do
    pcv <- loadCpu pc
    addrV <- readCpuMemory16 (pcv + 1)
    pure (False, addrV)
  AbsoluteX -> do
    pcv <- loadCpu pc
    xv <- loadCpu x
    v <- readCpuMemory16 (pcv + 1)
    let addrV = v + toWord16 xv
    let pageCrossed = differentPages (addrV - toWord16 xv) addrV
    pure (pageCrossed, addrV)
  AbsoluteY -> do
    pcv <- loadCpu pc
    yv <- loadCpu y
    v <- readCpuMemory16 (pcv + 1)
    let addrV = v + toWord16 yv
    let pageCrossed = differentPages (addrV - toWord16 yv) addrV
    pure (pageCrossed, addrV)
  Accumulator ->
    pure (False, 0)
  Immediate -> do
    pcv <- loadCpu pc
    pure (False, pcv + 1)
  Implied ->
    pure (False, 0)
  Indirect -> do
    pcv <- loadCpu pc
    addr <- readCpuMemory16 (pcv + 1)
    yo <- read16Bug addr
    pure (False, yo)
  IndirectIndexed -> do
    pcv <- loadCpu pc
    yv <- loadCpu y
    v <- readCpuMemory8 $ pcv + 1
    addr <- read16Bug $ toWord16 v
    let addrV = addr + toWord16 yv
    let pageCrossed = differentPages (addrV - toWord16 yv) addrV
    pure (pageCrossed, addrV)
  IndexedIndirect -> do
    pcv <- loadCpu pc
    xv <- loadCpu x
    v <- readCpuMemory8 $ pcv + 1
    addrV <- read16Bug $ toWord16 (v + xv)
    pure (False, addrV)
  Relative -> do
    pcv <- loadCpu pc
    offset16 <- readCpuMemory16 (pcv + 1)
    let offset8 = firstNibble offset16
    let diff = if offset8 < 0x80 then 0 else 0x100
    pure (False, pcv + 2 + offset8 - diff)
  ZeroPage -> do
    pcv <- loadCpu pc
    v <- readCpuMemory8 (pcv + 1)
    pure (False, toWord16 v)
  ZeroPageX -> do
    pcv <- loadCpu pc
    xv <- loadCpu x
    v <- readCpuMemory8 (pcv + 1)
    pure (False, toWord16 $ v + xv)
  ZeroPageY -> do
    pcv <- loadCpu pc
    yv <- loadCpu y
    v <- readCpuMemory8 (pcv + 1)
    pure (False, toWord16 $ v + yv)

differentPages :: Word16 -> Word16 -> Bool
differentPages a b = (a .&. 0xFF00) /= (b .&. 0xFF00)

incrementPc :: Opcode -> Emulator ()
incrementPc opcode = modifyCpu pc (+ instrLength)
  where instrLength = fromIntegral $ len opcode

getCycles :: Opcode -> Bool -> Int
getCycles opcode pageCrossed = if pageCrossed
  then pageCrossCycles opcode + cycles opcode
  else cycles opcode

handleInterrupts :: Emulator ()
handleInterrupts = do
  int <- loadCpu interrupt
  case int of
    Just NMI -> nmi
    Just IRQ -> error "not handling IRQ yet"
    Nothing  -> pure ()
  storeCpu interrupt Nothing

runInstruction :: Opcode -> (Word16 -> Emulator ())
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
  BRK -> const brk
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
  TAX -> const tax
  TAY -> const tay
  TSX -> const tsx
  TXA -> const txa
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
  ANC -> anc
  ALR -> alr
  ARR -> arr
  XAA -> const $ illegal mnemonic
  AHX -> const $ illegal mnemonic
  TAS -> const $ illegal mnemonic
  SHX -> shx
  SHY -> shy
  LAS -> const $ illegal mnemonic
  AXS -> axs

-- Official instructions

-- ADC - Add with carry
adc :: Word16 -> Emulator ()
adc addr = do
  av <- loadCpu a
  bv <- readCpuMemory8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  storeCpu a (av + bv + cv)
  av' <- loadCpu a
  setZN av'
  let shouldCarry = toInt av + toInt bv + toInt cv > 0xFF
  let doesOverflow = ((av `xor` bv) .&. 0x80) == 0 && ((av `xor` av') .&. 0x80) /= 0
  setFlag Carry shouldCarry
  setFlag Overflow doesOverflow

-- ASL - Arithmetic shift left
asl :: AddressMode -> Word16 -> Emulator ()
asl mode addr = do
  v <- case mode of
      Accumulator -> loadCpu a
      _           -> readCpuMemory8 addr
  let i = (v `shiftR` 7) .&. 1
  setFlag Carry (toEnum . fromIntegral $ i)
  let shiftedV = v `shiftL` 1
  case mode of
      Accumulator -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
  setZN shiftedV

-- AND - Logical and
and :: Word16 -> Emulator ()
and addr = do
  av <- loadCpu a
  v <- readCpuMemory8 addr
  storeCpu a (av .&. v)
  av' <- loadCpu a
  setZN av'

-- BCC - Branch on carry flag clear
bcc :: Word16 -> Emulator ()
bcc = branch $ not <$> getFlag Carry

-- BCS - Branch on carry flag set
bcs :: Word16 -> Emulator ()
bcs = branch $ getFlag Carry

-- BEQ - Branch if zero set
beq :: Word16 -> Emulator ()
beq = branch $ getFlag Zero

-- BMI - Branch if minus
bmi :: Word16 -> Emulator ()
bmi = branch $ getFlag Negative

-- BPL - Branch if positive
bpl :: Word16 -> Emulator ()
bpl = branch $ not <$> getFlag Negative

-- BVS - Branch if overflow clear
bvc :: Word16 -> Emulator ()
bvc = branch $ not <$> getFlag Overflow

-- BVS - Branch if overflow set
bvs :: Word16 -> Emulator ()
bvs = branch $ getFlag Overflow

-- BRK - Force interrupt
brk :: Emulator ()
brk = do
  pcv <- loadCpu pc
  push16 $ pcv + 1
  php
  sei
  av <- readCpuMemory16 0xFFFE
  storeCpu pc av

-- BIT - Test Bits in memory with A
bit :: Word16 -> Emulator ()
bit addr = do
  v <- readCpuMemory8 addr
  -- setV $ (v `shiftR` 6) .&. 1
  av <- loadCpu a
  setZ (v .&. av)
  setV v
  setN v

-- BNE - Branch if zero not set
bne :: Word16 -> Emulator ()
bne = branch $ not <$> getFlag Zero

-- CLC - Clear carry flag
clc :: Emulator ()
clc = setFlag Carry False

-- CLD - Clear decimal flag
cld :: Emulator ()
cld = setFlag Decimal False

-- CLI - Clear interrupt disable flag
cli :: Emulator ()
cli = setFlag InterruptDisable False

-- CLV - Clear overflow flag
clv :: Emulator ()
clv = setFlag Overflow False

-- CMP - Compare memory and A
cmp :: Word16 -> Emulator ()
cmp addr = do
  v <- readCpuMemory8 addr
  av <- loadCpu a
  compare av v

-- CPX - Compare memory and X
cpx :: Word16 -> Emulator ()
cpx addr = do
  v <- readCpuMemory8 addr
  xv <- loadCpu x
  compare xv v

-- CPY - Compare memory and Y
cpy :: Word16 -> Emulator ()
cpy addr = do
  v <- readCpuMemory8 addr
  yv <- loadCpu y
  compare yv v

-- DEC - Decrement memory
dec :: Word16 -> Emulator ()
dec addr = do
  v <- readCpuMemory8 addr
  let value = v - 1
  writeCpuMemory8 addr value
  setZN value

-- DEX - Decrement X register
dex :: Emulator ()
dex = do
  v <- loadCpu x
  let value = v - 1
  storeCpu x value
  setZN value


-- DEY - Decrement Y register
dey :: Emulator ()
dey = do
  v <- loadCpu y
  let value = v - 1
  storeCpu y value
  setZN value

-- EOR - Exclusive or
eor :: Word16 -> Emulator ()
eor addr = do
  v <- readCpuMemory8 addr
  av <- loadCpu a
  let newAv = av `xor` v
  storeCpu a newAv
  setZN newAv


-- INC - Increment memory
inc :: Word16 -> Emulator ()
inc addr = do
  v <- readCpuMemory8 addr
  let value = v + 1
  writeCpuMemory8 addr value
  setZN value

-- INX - Increment X register
inx :: Emulator ()
inx  = do
  v <- loadCpu x
  let value = v + 1
  storeCpu x value
  setZN value

-- INY - Increment Y register
iny :: Emulator ()
iny  = do
  v <- loadCpu y
  let value = v + 1
  storeCpu y value
  setZN value

-- JMP - Move execution to a particular address
jmp :: Word16 -> Emulator ()
jmp = storeCpu pc

-- JSR - Jump to subroutine
jsr :: Word16 -> Emulator ()
jsr addr = do
  pcv <- loadCpu pc
  push16 $ pcv - 1
  storeCpu pc addr

-- LDA - Load accumulator register
lda :: Word16 -> Emulator ()
lda addr = do
  v <- readCpuMemory8 addr
  storeCpu a v
  setZN v

-- LDX - Load X Register
ldx :: Word16 -> Emulator ()
ldx addr = do
  v <- readCpuMemory8 addr
  storeCpu x v
  setZN v

-- LDY - Load Y Register
ldy :: Word16 -> Emulator ()
ldy addr = do
  v <- readCpuMemory8 addr
  storeCpu y v
  setZN v

-- LSR - Logical shift right
lsr :: AddressMode -> Word16 -> Emulator ()
lsr mode addr = do
  v <- case mode of
      Accumulator -> loadCpu a
      _           -> readCpuMemory8 addr

  setFlag Carry (toEnum . fromIntegral $ v .&. 1)
  let shiftedV = v `shiftR` 1
  case mode of
      Accumulator -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
  setZN shiftedV


-- NOP - No operation. Do nothing :D
nop :: Emulator ()
nop = pure ()

-- PHA - Push Accumulator register
pha :: Emulator ()
pha = do
  av <- loadCpu a
  push av

-- PHP - Push processor status onto stack
php :: Emulator ()
php = do
  p <- loadCpu p
  push $ p .|. 0x10

-- PLA - Pull Accumulator register
pla :: Emulator ()
pla = do
  v <- pull
  storeCpu a v
  setZN v

-- PLP - Pull Accumulator register
plp :: Emulator ()
plp = do
  v <- pull
  storeCpu p ((v .&. 0xEF) .|. 0x20)

-- ORA - Logical Inclusive OR
ora :: Word16 -> Emulator ()
ora addr = do
  v <- readCpuMemory8 addr
  av <- loadCpu a
  let newAv = av .|. v
  storeCpu a newAv
  setZN newAv

-- ROL - Rotate left
rol :: AddressMode -> Word16 -> Emulator ()
rol mode addr = do
  v <- case mode of
      Accumulator -> loadCpu a
      _           -> readCpuMemory8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  setFlag Carry (toEnum $ fromIntegral $ (v `shiftR` 7) .&. 1)
  let shiftedV = (v `shiftL` 1) .|. cv
  case mode of
      Accumulator -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
  setZN shiftedV

-- ROR - Rotate right
ror :: AddressMode -> Word16 -> Emulator ()
ror mode addr = do
  v <- case mode of
      Accumulator -> loadCpu a
      _           -> readCpuMemory8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  setFlag Carry (toEnum $ fromIntegral $ v .&. 1)
  let shiftedV = (v `shiftR` 1) .|. (cv `shiftL` 7)
  case mode of
      Accumulator -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
  setZN shiftedV

-- RTI - Return from interrupt
rti :: Emulator ()
rti = do
  addr <- pull
  storeCpu p (addr .&. 0xEf .|. 0x20)
  addr' <- pull16
  storeCpu pc addr'

-- RTS - Return from a subroutine
rts :: Emulator ()
rts = do
  addr <- pull16
  storeCpu pc (addr + 1)

-- SBC - Subtract with carry
sbc :: Word16 -> Emulator ()
sbc addr = do
  av <- loadCpu a
  bv <- readCpuMemory8 addr
  cv <- (fromIntegral . fromEnum) <$> getFlag Carry
  storeCpu a (av - bv - (1 - cv))
  av' <- loadCpu a
  setZN av'
  let shouldCarry = toInt av - toInt bv - toInt (1 - cv) >= 0
  let doesOverflow = ((av `xor` bv) .&. 0x80) /= 0 && ((av `xor` av') .&. 0x80) /= 0
  setFlag Carry shouldCarry
  setFlag Overflow doesOverflow

-- SEC - Set carry flag
sec :: Emulator ()
sec = setFlag Carry True

-- SED - Set decimal flag
sed :: Emulator ()
sed = setFlag Decimal True

-- SEI - Set interrupt disable flag
sei :: Emulator ()
sei = setFlag InterruptDisable True

-- STA - Store Accumulator register
sta :: Word16 -> Emulator ()
sta addr = do
  av <- loadCpu a
  writeCpuMemory8 addr av

-- STX - Store X register
stx :: Word16 -> Emulator ()
stx addr = loadCpu x >>= writeCpuMemory8 addr

-- STY - Store Y register
sty :: Word16 -> Emulator ()
sty addr = loadCpu y >>= writeCpuMemory8 addr

-- TAX - Transfer Accumulator to X
tax :: Emulator ()
tax = do
  av <- loadCpu a
  storeCpu x av
  xv <- loadCpu x
  setZN xv

-- TAY - Transfer Accumulator to Y
tay :: Emulator ()
tay =  do
  av <- loadCpu a
  storeCpu y av
  yv <- loadCpu y
  setZN yv

-- TSX - Transfer Stack Pointer to X
tsx :: Emulator ()
tsx = do
  spv <- loadCpu sp
  storeCpu x spv
  xv <- loadCpu x
  setZN xv

-- TXA - Transfer X to Accumulator
txa :: Emulator ()
txa = do
  xv <- loadCpu x
  storeCpu a xv
  av <- loadCpu a
  setZN av

-- TXS - Transfer X to Stack Pointer
txs :: Emulator ()
txs = do
  xv <- loadCpu x
  storeCpu sp xv

-- TYA - Transfer Y to Accumulator
tya :: Emulator ()
tya = do
  yv <- loadCpu y
  storeCpu a yv
  av <- loadCpu a
  setZN av

-- Illegal instructions:
-- See: http://www.ffd2.com/fridge/docs/6502-NMOS.extra.opcodes

-- ALR/ASR - This opcode ANDs the contents of the A register with an immediate value and
-- then LSRs the result.
alr :: Word16 -> Emulator ()
alr addr = do
  and addr
  lsr Accumulator addr

-- ANC/AAC - ANC ANDs the contents of the A register with an immediate value and then
-- moves bit 7 of A into the Carry flag.  This opcode works basically
-- identically to AND #immed. except that the Carry flag is set to the same
-- state that the Negative flag is set to.
anc :: Word16 -> Emulator ()
anc addr = do
  av <- loadCpu a
  v <- readCpuMemory8 addr
  storeCpu a (av .&. v)
  av' <- loadCpu a
  setZN av'
  z <- getFlag Negative
  setFlag Carry z

-- ARR - This opcode ANDs the contents of the A register with an immediate value and
-- then RORs the result.
arr :: Word16 -> Emulator ()
arr addr = do
  and addr
  ror Accumulator addr
  a <- loadCpu a
  let bit5 = (a `shiftR` 5) .&. 0x1 == 1
  let bit6 = (a `shiftR` 6) .&. 0x1 == 1
  setFlag Carry bit6
  setFlag Overflow (bit6 `xor` bit5)

-- AXS - AXS ANDs the contents of the A and X registers (without changing the
-- contents of either register) and stores the result in memory.
-- AXS does not affect any flags in the processor status register.
-- See
axs :: Word16 -> Emulator ()
axs addr = do
  av <- loadCpu a
  xv <- loadCpu x
  v <- readCpuMemory8 addr
  let anded = av .&. xv
  let newXv = anded - v
  storeCpu x newXv
  setFlag Carry (anded >= v)
  setZN newXv

-- LAX - Load Accumulator and X with memory
lax :: Word16 -> Emulator ()
lax addr = do
  v <- readCpuMemory8 addr
  storeCpu a v
  storeCpu x v
  setZN v

-- SAX - AND X register with Accumulator and store result in memory
sax :: Word16 -> Emulator ()
sax addr = do
  av <- loadCpu a
  xv <- loadCpu x
  writeCpuMemory8 addr (av .&. xv)

-- DCP - Subtract 1 from memory
dcp :: Word16 -> Emulator ()
dcp addr = dec addr >> cmp addr

-- ISC - INCs the contents of a memory location and then SBCs the result
-- from the A register.
isc :: Word16 -> Emulator ()
isc addr = inc addr >> sbc addr

-- RLA - ROLs the contents of a memory location and then ANDs the result with
-- the Accumulator.
rla :: AddressMode -> Word16 -> Emulator ()
rla mode addr = rol mode addr >> and addr

-- SLO - ASLs the contents of a memory location and then ORs the result
-- with the Accumulator.
slo :: AddressMode -> Word16 -> Emulator ()
slo mode addr = asl mode addr >> ora addr

-- SRE - LSRs the contents of a memory location and then EORs the result with
-- the Accumulator.
sre :: AddressMode -> Word16 -> Emulator ()
sre mode addr = lsr mode addr >> eor addr

-- RRA - RORs the contents of a memory location and then ADCs the result with
-- the Accumulator
rra :: AddressMode -> Word16 -> Emulator ()
rra mode addr = ror mode addr >> adc addr

shx :: Word16 -> Emulator ()
shx addr = do
  xv <- loadCpu x
  yv <- loadCpu y
  let result = xv .&. (toWord8 addr + 1)
  let temp = (toWord8 addr - yv) .&. 255
  if yv + temp <= 255
    then writeCpuMemory8 addr result
    else do
      v <- readCpuMemory8 addr
      writeCpuMemory8 addr v


shy :: Word16 -> Emulator ()
shy addr = pure ()

-- NMI - Non Maskable Interrupt. Not strictly an opcode, but can represented as one
nmi :: Emulator ()
nmi = do
  pcv <- loadCpu pc
  push16 pcv
  php
  v <- readCpuMemory16 0xFFFA
  storeCpu pc v

-- Moves execution to addr if condition is set
branch :: Emulator Bool -> Word16 -> Emulator ()
branch cond addr = do
  cv <- cond
  pcv <- loadCpu pc
  when cv $ do
    storeCpu pc addr
    let cycles = if differentPages pcv addr then 2 else 1
    addCycles cycles

read16Bug :: Word16 -> Emulator Word16
read16Bug addr = do
  lo <- readCpuMemory8 addr
  hi <- readCpuMemory8 $ (addr .&. 0xFF00) .|. toWord16 (toWord8 addr + 1)
  pure $ makeW16 lo hi

pull :: Emulator Word8
pull = do
  spv <- loadCpu sp
  storeCpu sp (spv + 1)
  let i = 0x100 .|. (toWord16 spv + 1)
  readCpuMemory8 i

pull16 :: Emulator Word16
pull16 = do
  lo <- pull
  hi <- pull
  pure $ makeW16 lo hi

push :: Word8 -> Emulator ()
push v = do
  spv <- loadCpu sp
  let i = 0x100 .|. toWord16 spv
  writeCpuMemory8 i v
  storeCpu sp (spv - 1)

push16 :: Word16 -> Emulator ()
push16 v = do
  let (lo, hi) = splitW16 v
  push hi
  push lo

getFlag :: Flag -> Emulator Bool
getFlag flag = do
  v <- loadCpu p
  pure $ testBit v (7 - fromEnum flag)

setFlag :: Flag -> Bool -> Emulator ()
setFlag flag b = do
  v <- loadCpu p
  storeCpu p (opBit v (7 - fromEnum flag))
  where opBit = if b then setBit else clearBit

-- Sets the zero flag
setZ :: Word8 -> Emulator ()
setZ v = setFlag Zero (v == 0)

-- Sets the negative flag
setN :: Word8 -> Emulator ()
setN v = setFlag Negative (v .&. 0x80 /= 0)

-- Sets the overflow flag
setV :: Word8 -> Emulator ()
setV v = setFlag Overflow (v .&. 0x40 /= 0)

-- Sets the zero flag and the negative flag
setZN :: Word8 -> Emulator ()
setZN v = setZ v >> setN v

compare :: Word8 -> Word8 -> Emulator ()
compare a b = do
  setZN $ a - b
  setFlag Carry (a >= b)

illegal :: Mnemonic -> Emulator ()
illegal mnem = pure ()

addCycles :: Int -> Emulator ()
addCycles n = modifyCpu cpuCycles (+ n)
