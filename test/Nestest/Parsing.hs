module Nestest.Parsing (
  parseTrace
) where

import           Data.Void
import           Data.Word
import           Emulator.Opcode
import           Emulator.Trace
import           Numeric            (readHex)
import           Text.Parsec
import           Text.Parsec.String (Parser)

parseT = parseTest parseTrace "C5F5  A2 00     LDX #$00                        A:00 X:00 Y:00 P:24 SP:FD CYC:  9 SL:241"

parseTrace :: Parser Trace
parseTrace = do
  pcv <- hexWord16
  _ <- spaces
  a0r <- hexWord8
  _ <- space
  a1r <- hexWord8
  _ <- space
  a2r <- hexWord8
  _ <- spaces
  mnem <- mnemonic
  _ <- spaces
  manyTill anyChar (count 4 space)
  _ <- spaces
  av <- string "A:" >> hexWord8
  _ <- space
  xv <- string "X:" >> hexWord8
  _ <- space
  yv <- string "Y:" >> hexWord8
  _ <- space
  pv <- string "P:" >> hexWord8
  _ <- space
  spv <- string "SP:" >> hexWord8
  let opcode = decodeOpcode a0r
  pure $ Trace pcv spv av xv yv pv opcode a0r a1r a2r

-- need to fix this
mnemonic :: Parser Mnemonic
mnemonic = read <$> count 3 upper

hexWord8 :: Parser Word8
hexWord8 = toHexValue <$> count 2 hexDigit

hexWord16 :: Parser Word16
hexWord16 = toHexValue <$> count 4 hexDigit

toHexValue :: (Num a, Eq a) => String -> a
toHexValue = fst . head . readHex
