module Blargg.Spec where

import           Control.Monad
import qualified Data.ByteString  as BS
import           Emulator
import           Emulator.Monad
import           Test.Tasty
import           Test.Tasty.HUnit

test :: TestTree
test = testGroup "blargg ppu" [
    paletteRam
  , powerUpPalette
  , spriteRam
  , vblClearTime
  , vramAccess
  ]

paletteRam :: TestTree
paletteRam = testCase "palette_ram" $
  runRom "roms/tests/ppu/blargg/palette_ram.nes"

powerUpPalette :: TestTree
powerUpPalette = testCase "power_up_palette" $
  runRom "roms/tests/ppu/blargg/power_up_palette.nes"

spriteRam :: TestTree
spriteRam = testCase "sprite_ram" $
  runRom "roms/tests/ppu/blargg/sprite_ram.nes"

vblClearTime :: TestTree
vblClearTime = testCase "vbl_clear_time" $
  runRom "roms/tests/ppu/blargg/vbl_clear_time.nes"

vramAccess :: TestTree
vramAccess = testCase "vram_access" $
  runRom "roms/tests/ppu/blargg/vram_access.nes"

runRom :: FilePath -> IO ()
runRom rom = do
  rom <- BS.readFile rom
  runIOEmulator rom stepFrame
