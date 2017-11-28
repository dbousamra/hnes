module Blargg.Spec where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator
import           Emulator.Monad
import           Emulator.Nes
import           Test.Tasty
import           Test.Tasty.HUnit

test :: TestTree
test = testGroup "blargg" [
    ppuTest
  , cpuTest
  ]

ppuTest :: TestTree
ppuTest = testGroup "ppu" [
    testCase "palette_ram"      $ run "roms/tests/ppu/blargg/palette_ram.nes"
  , testCase "power_up_palette" $ run "roms/tests/ppu/blargg/power_up_palette.nes"
  , testCase "sprite_ram"       $ run "roms/tests/ppu/blargg/sprite_ram.nes"
  , testCase "vbl_clear_time"   $ run "roms/tests/ppu/blargg/vbl_clear_time.nes"
  , testCase "vram_access"      $ run "roms/tests/ppu/blargg/vram_access.nes"
  ]

cpuTest :: TestTree
cpuTest = testGroup "cpu" [
    testCase "basics"    $ run "roms/tests/cpu/blargg/01-basics.nes"
  , testCase "implied"   $ run "roms/tests/cpu/blargg/02-implied.nes"
  , testCase "immediate" $ run "roms/tests/cpu/blargg/03-immediate.nes"
  , testCase "zero page" $ run "roms/tests/cpu/blargg/04-zero_page.nes"
  , testCase "zp xy"     $ run "roms/tests/cpu/blargg/05-zp_xy.nes"
  , testCase "absolute"  $ run "roms/tests/cpu/blargg/06-absolute.nes"
  -- , testCase "abs xy"    $ run "roms/tests/cpu/blargg/07-abs_xy.nes"
  , testCase "ind x"     $ run "roms/tests/cpu/blargg/08-ind_x.nes"
  , testCase "ind y"     $ run "roms/tests/cpu/blargg/09-ind_y.nes"
  , testCase "branches"  $ run "roms/tests/cpu/blargg/10-branches.nes"
  , testCase "stack"     $ run "roms/tests/cpu/blargg/11-stack.nes"
  , testCase "jmp jsr"   $ run "roms/tests/cpu/blargg/12-jmp_jsr.nes"
  , testCase "rts"       $ run "roms/tests/cpu/blargg/13-rts.nes"
  , testCase "rti"       $ run "roms/tests/cpu/blargg/14-rti.nes"
  , testCase "brk"       $ run "roms/tests/cpu/blargg/15-brk.nes"
  , testCase "special"   $ run "roms/tests/cpu/blargg/16-special.nes"
  ]

run :: FilePath -> IO ()
run rom = do
  rom <- BS.readFile rom
  runIOEmulator rom $ do
    reset
    replicateM_ 300 stepFrame
    result <- load (Cpu $ CpuMemory8 0x6000)
    liftIO $ assertEqual "Return code at 0x6000" 0 result
