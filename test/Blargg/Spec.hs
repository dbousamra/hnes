module Blargg.Spec where

import           Control.Monad
import qualified Data.ByteString  as BS
import           Emulator
import           Emulator.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec      (parse)

test :: TestTree
test = testGroup "blargg" [
    paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  , paletteRam
  ]

paletteRam :: TestTree
paletteRam = testCase "palette_ram" $
  runStuff

runStuff :: IO ()
runStuff = do
  rom <- BS.readFile "roms/tests/ppu/blargg/palette_ram.nes"
  runIOEmulator rom stepFrame
