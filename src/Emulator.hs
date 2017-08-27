module Emulator () where

import           Cartridge
import           Control.Monad.IO.Class
import           Data.Bits              ((.|.))
import           Data.ByteString        as BS hiding (putStrLn, replicate, take,
                                               zip)
import           Data.Word
import           Monad
import           Nes                    (Address (..))
import           Opcode
import           Util

run :: FilePath -> IO ()
run fp = do
  cart <- parseCartridge <$> readBytes fp
  runIOEmulator cart $ do
    store Pc 0xC000
    emulate 0 10

r :: IO ()
r = run "roms/nestest.nes"

readBytes :: FilePath -> IO ByteString
readBytes = BS.readFile

loadNextOpcode :: (MonadIO m, MonadEmulator m) => m Opcode
loadNextOpcode = do
  pc <- load Pc
  pcv <- load (Ram8 pc)
  pure $ decodeOpcode pcv

emulate :: (MonadIO m, MonadEmulator m) => Int -> Int -> m ()
emulate n max =
  if n >= max then pure ()
  else do
    opcode <- loadNextOpcode
    execute opcode
    emulate (n + 1) max

incrementPc :: MonadEmulator m => Word16 -> m ()
incrementPc n = do
  pc <- load Pc
  store Pc (pc + n)

addressForMode :: (MonadIO m, MonadEmulator m) => AddressMode -> m Word16
addressForMode Absolute = do
  pcv <- load Pc
  load $ Ram16 (pcv + 1)
addressForMode Immediate = do
  pcv <- load Pc
  pure $ pcv + 1
addressForMode ZeroPage = do
  pcv <- load Pc
  v <- load $ Ram8 (pcv + 1)
  pure $ toWord16 v
addressForMode mode = error $ "Unimplemented AddressMode " ++ (show mode)

pcIncrementForOpcode :: Opcode -> Word16
pcIncrementForOpcode (Opcode _ mn mode) = case (mode, mn) of
  (_, JMP)             -> 0
  (_, JSR)             -> 0
  (_, RTS)             -> 0
  (_, RTI)             -> 0
  (Indirect, _)        -> 0
  (Relative, _)        -> 0
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
  pcv <- load Pc
  spv <- load Sp
  liftIO $ putStrLn $ "PC: " ++ (prettifyWord16 pcv) ++ " " ++ (show op) ++ " SP: " ++ (prettifyWord8 spv)
  addr <- addressForMode mode
  go addr
  incrementPc $ pcIncrementForOpcode op
  where
    go = case mn of
      JMP   -> jmp
      JSR   -> jsr
      LDX   -> ldx
      STX   -> stx
      other -> error $ "Unimplemented opcode: " ++ (show other)

push :: MonadEmulator m => Word8 -> m ()
push v = do
  spv <- load Sp
  store (Ram8 $ 0x100 .|. (toWord16 spv)) v
  store Sp (spv - 1)

push16 :: MonadEmulator m => Word16 -> m ()
push16 v = do
  let (lo, hi) = splitW16 v
  push hi
  push lo

-- JMP - Move execution to a particular address
jmp :: MonadEmulator m => Word16 -> m ()
jmp = store Pc

-- JSR - Jump to subroutine
jsr :: MonadEmulator m => Word16 -> m ()
jsr addr = do
  pcv <- load Pc
  push16 $ pcv - 1
  store Pc addr

-- LDX - Load X Register
ldx :: MonadEmulator m => Word16 -> m ()
ldx addr = do
  v <- load $ Ram8 addr
  store X v
  -- TODO: set ZN flag

-- STX - Store X Register
stx :: MonadEmulator m => Word16 -> m ()
stx addr = do
  xv <- load X
  store (Ram8 addr) xv


renderEmulator :: MonadEmulator m => m String
renderEmulator = do
  pcv <- load Pc
  spv <- load Sp
  xv  <- load X
  yv  <- load Y
  pure $ "PC: " ++ (prettifyWord16 pcv) ++ " " ++
         "SP: " ++ (prettifyWord8 spv) ++ " " ++
         "X: " ++ (prettifyWord8 xv) ++ " " ++
         "Y: " ++ (prettifyWord8 xv) ++ " "

