module Emulator.Opcode (
    Mnemonic(..)
  , AddressMode(..)
  , Opcode(..)
  , decodeOpcode
) where

import           Data.List              (intersperse)
import           Data.Word
import           Emulator.Util

data AddressMode
  = Implied
  | Accumulator
  | Immediate
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Relative
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Indirect
  | IndexedIndirect
  | IndirectIndexed
    deriving (Show, Eq)

data Mnemonic
-- Official - 47
  = ADC | AND | ASL | BCC | BCS | BEQ
  | BIT | BMI | BNE | BPL | BRK | BVC
  | BVS | CLC | CLD | CLI | CLV | CMP
  | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA
  | LDX | LDY | LSR | NOP | ORA | PHA
  | PHP | PLA | PLP | ROL | ROR | RTI
  | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA
  | TXS | TYA
  -- Illegal / Unofficial - 19
  | KIL | LAX | SAX | DCP | ISC | RLA
  | RRA | SLO | SRE | ANC | ALR | ARR
  | XAA | AHX | TAS | SHX | SHY | LAS
  | AXS
    deriving (Show, Read, Eq, Enum)

data Opcode = Opcode {
  raw             :: Word8,
  mnem            :: Mnemonic,
  mode            :: AddressMode,
  len             :: Int,
  cycles          :: Int,
  pageCrossCycles :: Int
} deriving (Eq)

instance Show Opcode where
  show (Opcode raw mn mode len cyc pageCrossCy) =
    concat . intersperse " "
      $ "Opcode:" : prettifyWord8 raw
      : show mn : show mode
      : map show [len, cyc, pageCrossCy]

-- see http://e-tradition.net/bytes/6502/6502_instruction_set.html
decodeOpcode :: Word8 -> Opcode
decodeOpcode w = Opcode w mnemonic addressMode length cycles pageCrossCycles
  where
    (mnemonic, addressMode, length, cycles, pageCrossCycles) = case w of
      0x69 -> (ADC, Immediate,       2, 2, 0); 0x65 -> (ADC, ZeroPage,        2, 3, 0); 0x75 -> (ADC, ZeroPageX,       2, 4, 0);
      0x6D -> (ADC, Absolute,        3, 4, 0); 0x7D -> (ADC, AbsoluteX,       3, 4, 1); 0x79 -> (ADC, AbsoluteY,       3, 4, 1);
      0x61 -> (ADC, IndexedIndirect, 2, 6, 0); 0x71 -> (ADC, IndirectIndexed, 2, 5, 1); 0x29 -> (AND, Immediate,       2, 2, 0);
      0x25 -> (AND, ZeroPage,        2, 3, 0); 0x35 -> (AND, ZeroPageX,       2, 4, 0); 0x2D -> (AND, Absolute,        3, 4, 0);
      0x3D -> (AND, AbsoluteX,       3, 4, 1); 0x39 -> (AND, AbsoluteY,       3, 4, 1); 0x21 -> (AND, IndexedIndirect, 2, 6, 0);
      0x31 -> (AND, IndirectIndexed, 2, 5, 1); 0x0A -> (ASL, Accumulator,     1, 2, 0); 0x06 -> (ASL, ZeroPage,        2, 5, 0);
      0x16 -> (ASL, ZeroPageX,       2, 6, 0); 0x0E -> (ASL, Absolute,        3, 6, 0); 0x1E -> (ASL, AbsoluteX,       3, 7, 0);
      0x90 -> (BCC, Relative,        2, 2, 0); 0xB0 -> (BCS, Relative,        2, 2, 0); 0xF0 -> (BEQ, Relative,        2, 2, 1);
      0x24 -> (BIT, ZeroPage,        2, 3, 0); 0x2C -> (BIT, Absolute,        3, 4, 0); 0x30 -> (BMI, Relative,        2, 2, 0);
      0xD0 -> (BNE, Relative,        2, 2, 0); 0x10 -> (BPL, Relative,        2, 2, 0); 0x00 -> (BRK, Implied,         1, 7, 0);
      0x50 -> (BVC, Relative,        2, 2, 0); 0x70 -> (BVS, Relative,        2, 2, 0); 0x18 -> (CLC, Implied,         1, 2, 0);
      0xD8 -> (CLD, Implied,         1, 2, 0); 0x58 -> (CLI, Implied,         1, 2, 0); 0xB8 -> (CLV, Implied,         1, 2, 0);
      0xC9 -> (CMP, Immediate,       2, 2, 0); 0xC5 -> (CMP, ZeroPage,        2, 3, 0); 0xD5 -> (CMP, ZeroPageX,       2, 4, 0);
      0xCD -> (CMP, Absolute,        3, 4, 0); 0xDD -> (CMP, AbsoluteX,       3, 4, 1); 0xD9 -> (CMP, AbsoluteY,       3, 4, 1);
      0xC1 -> (CMP, IndexedIndirect, 2, 6, 0); 0xD1 -> (CMP, IndirectIndexed, 2, 5, 1); 0xE0 -> (CPX, Immediate,       2, 2, 0);
      0xE4 -> (CPX, ZeroPage,        2, 3, 0); 0xEC -> (CPX, Absolute,        3, 4, 0); 0xC0 -> (CPY, Immediate,       2, 2, 0);
      0xC4 -> (CPY, ZeroPage,        2, 3, 0); 0xCC -> (CPY, Absolute,        3, 4, 0); 0xC6 -> (DEC, ZeroPage,        2, 5, 0);
      0xD6 -> (DEC, ZeroPageX,       2, 6, 0); 0xCE -> (DEC, Absolute,        3, 6, 0); 0xDE -> (DEC, AbsoluteX,       3, 7, 0);
      0xCA -> (DEX, Implied,         1, 2, 0); 0x88 -> (DEY, Implied,         1, 2, 0); 0x49 -> (EOR, Immediate,       2, 2, 0);
      0x45 -> (EOR, ZeroPage,        2, 3, 0); 0x55 -> (EOR, ZeroPageX,       2, 4, 0); 0x4D -> (EOR, Absolute,        3, 4, 0);
      0x5D -> (EOR, AbsoluteX,       3, 4, 1); 0x59 -> (EOR, AbsoluteY,       3, 4, 1); 0x41 -> (EOR, IndexedIndirect, 2, 6, 0);
      0x51 -> (EOR, IndirectIndexed, 2, 5, 1); 0xE6 -> (INC, ZeroPage,        2, 5, 0); 0xF6 -> (INC, ZeroPageX,       2, 6, 0);
      0xEE -> (INC, Absolute,        3, 6, 0); 0xFE -> (INC, AbsoluteX,       3, 7, 0); 0xE8 -> (INX, Implied,         1, 2, 0);
      0xC8 -> (INY, Implied,         1, 2, 0); 0x4C -> (JMP, Absolute,        3, 3, 0); 0x6C -> (JMP, Indirect,        3, 5, 0);
      0x20 -> (JSR, Absolute,        3, 6, 0); 0xA9 -> (LDA, Immediate,       2, 2, 0); 0xA5 -> (LDA, ZeroPage,        2, 3, 0);
      0xB5 -> (LDA, ZeroPageX,       2, 4, 0); 0xAD -> (LDA, Absolute,        3, 4, 0); 0xBD -> (LDA, AbsoluteX,       3, 4, 1);
      0xB9 -> (LDA, AbsoluteY,       3, 4, 1); 0xA1 -> (LDA, IndexedIndirect, 2, 6, 0); 0xB1 -> (LDA, IndirectIndexed, 2, 5, 1);
      0xA2 -> (LDX, Immediate,       2, 2, 0); 0xA6 -> (LDX, ZeroPage,        2, 3, 0); 0xB6 -> (LDX, ZeroPageY,       2, 4, 0);
      0xAE -> (LDX, Absolute,        3, 4, 0); 0xBE -> (LDX, AbsoluteY,       3, 4, 1); 0xA0 -> (LDY, Immediate,       2, 2, 0);
      0xA4 -> (LDY, ZeroPage,        2, 3, 0); 0xB4 -> (LDY, ZeroPageX,       2, 4, 0); 0xAC -> (LDY, Absolute,        3, 4, 0);
      0xBC -> (LDY, AbsoluteX,       3, 4, 1); 0x4A -> (LSR, Accumulator,     1, 2, 0); 0x46 -> (LSR, ZeroPage,        2, 5, 0);
      0x56 -> (LSR, ZeroPageX,       2, 6, 0); 0x4E -> (LSR, Absolute,        3, 6, 0); 0x5E -> (LSR, AbsoluteX,       3, 7, 0);
      0xEA -> (NOP, Implied,         1, 2, 0); 0x09 -> (ORA, Immediate,       2, 2, 0); 0x05 -> (ORA, ZeroPage,        2, 3, 0);
      0x15 -> (ORA, ZeroPageX,       2, 4, 0); 0x0D -> (ORA, Absolute,        3, 4, 0); 0x1D -> (ORA, AbsoluteX,       3, 4, 1);
      0x19 -> (ORA, AbsoluteY,       3, 4, 1); 0x01 -> (ORA, IndexedIndirect, 2, 6, 0); 0x11 -> (ORA, IndirectIndexed, 2, 5, 1);
      0x48 -> (PHA, Implied,         1, 3, 0); 0x08 -> (PHP, Implied,         1, 3, 0); 0x68 -> (PLA, Implied,         1, 4, 0);
      0x28 -> (PLP, Implied,         1, 4, 0); 0x2A -> (ROL, Accumulator,     1, 2, 0); 0x26 -> (ROL, ZeroPage,        2, 5, 0);
      0x36 -> (ROL, ZeroPageX,       2, 6, 0); 0x2E -> (ROL, Absolute,        3, 6, 0); 0x3E -> (ROL, AbsoluteX,       3, 7, 0);
      0x6A -> (ROR, Accumulator,     1, 2, 0); 0x66 -> (ROR, ZeroPage,        2, 5, 0); 0x76 -> (ROR, ZeroPageX,       2, 6, 0);
      0x6E -> (ROR, Absolute,        3, 6, 0); 0x7E -> (ROR, AbsoluteX,       3, 7, 0); 0x40 -> (RTI, Implied,         1, 6, 0);
      0x60 -> (RTS, Implied,         1, 6, 0); 0xE9 -> (SBC, Immediate,       2, 2, 0); 0xE5 -> (SBC, ZeroPage,        2, 3, 0);
      0xF5 -> (SBC, ZeroPageX,       2, 4, 0); 0xED -> (SBC, Absolute,        3, 4, 0); 0xFD -> (SBC, AbsoluteX,       3, 4, 1);
      0xF9 -> (SBC, AbsoluteY,       3, 4, 1); 0xE1 -> (SBC, IndexedIndirect, 2, 6, 0); 0xF1 -> (SBC, IndirectIndexed, 2, 5, 1);
      0x38 -> (SEC, Implied,         1, 2, 0); 0xF8 -> (SED, Implied,         1, 2, 0); 0x78 -> (SEI, Implied,         1, 2, 0);
      0x85 -> (STA, ZeroPage,        2, 3, 0); 0x95 -> (STA, ZeroPageX,       2, 4, 0); 0x8D -> (STA, Absolute,        3, 4, 0);
      0x9D -> (STA, AbsoluteX,       3, 5, 0); 0x99 -> (STA, AbsoluteY,       3, 5, 0); 0x81 -> (STA, IndexedIndirect, 2, 6, 0);
      0x91 -> (STA, IndirectIndexed, 2, 6, 0); 0x86 -> (STX, ZeroPage,        2, 3, 0); 0x96 -> (STX, ZeroPageY,       2, 4, 0);
      0x8E -> (STX, Absolute,        3, 4, 0); 0x84 -> (STY, ZeroPage,        2, 3, 0); 0x94 -> (STY, ZeroPageX,       2, 4, 0);
      0x8C -> (STY, Absolute,        3, 4, 0); 0xAA -> (TAX, Implied,         1, 2, 0); 0xA8 -> (TAY, Implied,         1, 2, 0);
      0xBA -> (TSX, Implied,         1, 2, 0); 0x8A -> (TXA, Implied,         1, 2, 0); 0x9A -> (TXS, Implied,         1, 2, 0);
      0x98 -> (TYA, Implied,         1, 2, 0);

      -- Illegal / Unofficial
      0x0B -> (ANC, Immediate,       2, 2, 0); 0x2B -> (ANC, Immediate,       2, 2, 0); 0x87 -> (SAX, ZeroPage,        2, 3, 0);
      0x97 -> (SAX, ZeroPageY,       2, 4, 0); 0x83 -> (SAX, IndexedIndirect, 2, 6, 0); 0x8F -> (SAX, Absolute,        3, 4, 0);
      0x6B -> (ARR, Immediate,       2, 2, 0); 0x4B -> (ALR, Immediate,       2, 2, 0); 0xAB -> (LAX, Immediate,       2, 2, 0);
      0x9F -> (AHX, AbsoluteY,       3, 5, 0); 0x93 -> (AHX, IndirectIndexed, 2, 6, 0); 0xCB -> (AXS, Immediate,       2, 2, 0);
      0xC7 -> (DCP, ZeroPage,        2, 5, 0); 0xD7 -> (DCP, ZeroPageX,       2, 6, 0); 0xCF -> (DCP, Absolute,        3, 6, 0);
      0xDF -> (DCP, AbsoluteX,       3, 7, 0); 0xDB -> (DCP, AbsoluteY,       3, 7, 0); 0xC3 -> (DCP, IndexedIndirect, 2, 8, 0);
      0x14 -> (NOP, ZeroPageX,       2, 4, 0); 0x34 -> (NOP, ZeroPageX,       2, 4, 0); 0x44 -> (NOP, ZeroPage,        2, 3, 0);
      0x54 -> (NOP, ZeroPageX,       2, 4, 0); 0x64 -> (NOP, ZeroPage,        2, 3, 0); 0x74 -> (NOP, ZeroPageX,       2, 4, 0);
      0x80 -> (NOP, Immediate,       2, 2, 0); 0x82 -> (NOP, Immediate,       2, 2, 0); 0x89 -> (NOP, Immediate,       2, 2, 0);
      0xC2 -> (NOP, Immediate,       2, 2, 0); 0xD4 -> (NOP, ZeroPageX,       2, 4, 0); 0xE2 -> (NOP, Immediate,       2, 2, 0);
      0xF4 -> (NOP, ZeroPageX,       2, 4, 0); 0xE7 -> (ISC, ZeroPage,        2, 5, 0); 0xF7 -> (ISC, ZeroPageX,       2, 6, 0);
      0xEF -> (ISC, Absolute,        3, 6, 0); 0xFF -> (ISC, AbsoluteX,       3, 7, 0); 0xFB -> (ISC, AbsoluteY,       3, 7, 0);
      0xE3 -> (ISC, IndexedIndirect, 2, 8, 0); 0xF3 -> (ISC, IndirectIndexed, 2, 8, 0); 0x02 -> (KIL, Implied,         1, 0, 0);
      0x12 -> (KIL, Implied,         1, 0, 0); 0x22 -> (KIL, Implied,         1, 0, 0); 0x32 -> (KIL, Implied,         1, 0, 0);
      0x42 -> (KIL, Implied,         1, 0, 0); 0x52 -> (KIL, Implied,         1, 0, 0); 0x62 -> (KIL, Implied,         1, 0, 0);
      0x72 -> (KIL, Implied,         1, 0, 0); 0x92 -> (KIL, Implied,         1, 0, 0); 0xB2 -> (KIL, Implied,         1, 0, 0);
      0xD2 -> (KIL, Implied,         1, 0, 0); 0xF2 -> (KIL, Implied,         1, 0, 0); 0xBB -> (LAS, AbsoluteY,       3, 4, 1);
      0xA7 -> (LAX, ZeroPage,        2, 3, 0); 0xB7 -> (LAX, ZeroPageY,       2, 4, 0); 0xAF -> (LAX, Absolute,        3, 4, 0);
      0xBF -> (LAX, AbsoluteY,       3, 4, 1); 0xA3 -> (LAX, IndexedIndirect, 2, 6, 0); 0xB3 -> (LAX, IndirectIndexed, 2, 5, 1);
      0x1A -> (NOP, Implied,         1, 2, 0); 0x3A -> (NOP, Implied,         1, 2, 0); 0x5A -> (NOP, Implied,         1, 2, 0);
      0x7A -> (NOP, Implied,         1, 2, 0); 0xDA -> (NOP, Implied,         1, 2, 0); 0xFA -> (NOP, Implied,         1, 2, 0);
      0x27 -> (RLA, ZeroPage,        2, 5, 0); 0x37 -> (RLA, ZeroPageX,       2, 6, 0); 0x2F -> (RLA, Absolute,        3, 6, 0);
      0x3F -> (RLA, AbsoluteX,       3, 7, 0); 0x3B -> (RLA, AbsoluteY,       3, 7, 0); 0x23 -> (RLA, IndexedIndirect, 2, 8, 0);
      0x33 -> (RLA, IndirectIndexed, 2, 8, 0); 0x67 -> (RRA, ZeroPage,        2, 5, 0); 0x77 -> (RRA, ZeroPageX,       2, 6, 0);
      0x6F -> (RRA, Absolute,        3, 6, 0); 0x7F -> (RRA, AbsoluteX,       3, 7, 0); 0x7B -> (RRA, AbsoluteY,       3, 7, 0);
      0x63 -> (RRA, IndexedIndirect, 2, 8, 0); 0x73 -> (RRA, IndirectIndexed, 2, 8, 0); 0xEB -> (SBC, Immediate,       2, 2, 0);
      0x07 -> (SLO, ZeroPage,        2, 5, 0); 0x17 -> (SLO, ZeroPageX,       2, 6, 0); 0x0F -> (SLO, Absolute,        3, 6, 0);
      0x1F -> (SLO, AbsoluteX,       3, 7, 0); 0x1B -> (SLO, AbsoluteY,       3, 7, 0); 0x03 -> (SLO, IndexedIndirect, 2, 8, 0);
      0x13 -> (SLO, IndirectIndexed, 2, 8, 0); 0x47 -> (SRE, ZeroPage,        2, 5, 0); 0x57 -> (SRE, ZeroPageX,       2, 6, 0);
      0x4F -> (SRE, Absolute,        3, 6, 0); 0x5F -> (SRE, AbsoluteX,       3, 7, 0); 0x5B -> (SRE, AbsoluteY,       3, 7, 0);
      0x43 -> (SRE, IndexedIndirect, 2, 8, 0); 0x53 -> (SRE, IndirectIndexed, 2, 8, 0); 0x9E -> (SHX, AbsoluteY,       3, 5, 0);
      0x9C -> (SHY, AbsoluteX,       3, 5, 0); 0x0C -> (NOP, Absolute,        3, 4, 0); 0x1C -> (NOP, AbsoluteX,       3, 4, 1);
      0x3C -> (NOP, AbsoluteX,       3, 4, 1); 0x5C -> (NOP, AbsoluteX,       3, 4, 1); 0x7C -> (NOP, AbsoluteX,       3, 4, 1);
      0xDC -> (NOP, AbsoluteX,       3, 4, 1); 0xFC -> (NOP, AbsoluteX,       3, 4, 1); 0xD3 -> (DCP, IndirectIndexed, 2, 8, 0);
      0x04 -> (NOP, ZeroPage,        2, 3, 0); 0x8B -> (XAA, Immediate,       2, 2, 0); 0x9B -> (TAS, AbsoluteY,       3, 5, 0);
      other -> error $ show other ++ " is not a known opcode"
