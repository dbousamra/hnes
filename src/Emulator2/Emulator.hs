module Emulator2.Emulator (
    step
  , run
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans    (MonadIO, lift)
import           Data.IORef
import qualified Emulator2.CPU          as CPU
import           Emulator2.Monad
import qualified Emulator2.PPU          as PPU

data NES = NES
  { cpu :: CPU.CPU
  , ppu :: PPU.PPU
  }

type NESEmulator b = Emulator NES b

loadCPU :: (CPU.CPU -> IORef b) -> NESEmulator b
loadCPU field = Emulator $ do
  thing <- ask
  lift $ readIORef $ field (cpu thing)

new :: IO NES
new = do
  cpu <- CPU.new
  ppu <- PPU.new
  pure $ NES cpu ppu

-- step :: NESEmulator ()
step = do
  pc <- loadCPU CPU.pc
  -- c <- CPU.step
  liftIO $ putStrLn $ show pc
  -- c' <- lift $ PPU.step
  pure ()
  -- s <- CPU.trace

main :: IO ()
-- main = run CPU.new $ replicateM_ 10000 step
main = run new step
