module Blargg.Spec where

import           Emulator.Nes     (readCpuMemory8, readPpuMemory)
import           SpecHelper
import           Test.Tasty
import           Test.Tasty.HUnit

test :: TestTree
test = testGroup "blargg" [
    cpuTest
  , ppuTest
  , vblTest
  ]

ppuTest :: TestTree
ppuTest = testGroup "ppu" [
    testCase "palette ram"      $ run "roms/tests/ppu/blargg/palette_ram.nes"
  -- , testCase "power up palette" $ run "roms/tests/ppu/blargg/power_up_palette.nes"
  , testCase "sprite ram"       $ run "roms/tests/ppu/blargg/sprite_ram.nes"
  , testCase "vbl clear time"   $ run "roms/tests/ppu/blargg/vbl_clear_time.nes"
  , testCase "vram access"      $ run "roms/tests/ppu/blargg/vram_access.nes"
  ]
  where
    run :: FilePath -> IO ()
    run filename = SpecHelper.run filename (readPpuMemory 0x20a4) 0x31

vblTest :: TestTree
vblTest = testGroup "vbl" [
    testCase "frame basics"     $ run "roms/tests/ppu/blargg_vbl/1.frame_basics.nes"
  -- , testCase "vbl timing"       $ run "roms/tests/ppu/blargg_vbl/2.vbl_timing.nes"
  -- , testCase "even odd frames"  $ run "roms/tests/ppu/blargg_vbl/3.even_odd_frames.nes"
  -- , testCase "vbl clear timing" $ run "roms/tests/ppu/blargg_vbl/4.vbl_clear_timing.nes"
  -- , testCase "nmi_suppression"  $ run "roms/tests/ppu/blargg_vbl/5.nmi_suppression.nes"
  -- , testCase "nmi disable"      $ run "roms/tests/ppu/blargg_vbl/6.nmi_disable.nes"
  -- , testCase "nmi timing"       $ run "roms/tests/ppu/blargg_vbl/7.nmi_timing.nes"
  ]
  where
    run :: FilePath -> IO ()
    run filename = SpecHelper.run filename (readPpuMemory 0x20C2) 0x50

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
  where
    run :: FilePath -> IO ()
    run filename = SpecHelper.run filename (readCpuMemory8 0x6000) 0x0
