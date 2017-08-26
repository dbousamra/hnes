module Emulator () where

import           Cartridge
import           Control.Monad.IO.Class
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
  v <- load $ Ram8(pcv + 1)
  pure $ toWord16 v
addressForMode mode = error $ "Unimplemented AddressMode " ++ (show mode)

execute :: (MonadIO m, MonadEmulator m) => Opcode -> m ()
execute op @ (Opcode _ mn mode) = do
  pc <- load Pc
  liftIO $ putStrLn $ "PC: " ++ (prettifyWord16 pc) ++ " " ++ (show op)
  addr <- addressForMode mode
  go addr
  where
    go = case mn of
      JMP   -> jmp
      LDX   -> ldx
      STX   -> stx
      other -> error $ "Unimplemented opcode: " ++ (show other)

-- Jump - Move execution to a particular address
jmp :: MonadEmulator m => Word16 -> m ()
jmp = store Pc

-- LDX - Load X Register
ldx :: MonadEmulator m => Word16 -> m ()
ldx addr = do
  v <- load $ Ram8 addr
  store X v
  incrementPc 2
  -- set ZN flag

-- STX - Store X Register
stx :: MonadEmulator m => Word16 -> m ()
stx addr = do
  xv <- load X
  store (Ram8 addr) xv



















