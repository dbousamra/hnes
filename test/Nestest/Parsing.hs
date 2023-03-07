module Nestest.Parsing (
  parseTrace
) where

import           Data.Word
import           Emulator.Opcode
import           Emulator.Trace
import           Numeric            (readHex)
import           Text.Parsec
import           Text.Parsec.String (Parser)

parseTrace :: Parser Trace
parseTrace = do
  pcv <- hexWord16
  _ <- spaces
  a0r <- hexWord8
  _ <- space
  a1r <- addr
  _ <- space
  a2r <- addr
  _ <- spaces
  _ <- count 32 anyChar
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
  _ <- space
  cyc <- string "CYC:" >> cyclesP
  _ <- space
  _ <- string "SL:" >> many (noneOf "\n")
  let opcode = decodeOpcode a0r
  pure $ Trace pcv spv av xv yv pv opcode a0r a1r a2r cyc

addr :: Parser Word8
addr = zeroAddr <|> hexWord8
  where zeroAddr = (count 2 space) >> (pure 0x0)

hexWord8 :: Parser Word8
hexWord8 = toHexValue <$> count 2 hexDigit

hexWord16 :: Parser Word16
hexWord16 = toHexValue <$> count 4 hexDigit

cyclesP :: Parser Int
cyclesP = read <$> (count 3 anyChar)

toHexValue :: (Num a, Eq a) => String -> a
toHexValue = fst . head . readHex
