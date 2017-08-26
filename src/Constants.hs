module Constants where

import           Data.Word

cpuRamBegin :: Word16
cpuRamBegin = 0x0000

cpuRamEnd :: Word16
cpuRamEnd = 0x07FF

ramMirrorsBegin :: Word16
ramMirrorsBegin = 0x0800

ramMirrorsEnd :: Word16
ramMirrorsEnd = 0x1FFF

ppuRegisterBegin :: Word16
ppuRegisterBegin = 0x2000

ppuRegisterEnd :: Word16
ppuRegisterEnd= 0x2007

ppuMirrorsBegin :: Word16
ppuMirrorsBegin = 0x2008

ppuMirrorsEnd :: Word16
ppuMirrorsEnd= 0x3FFF

ioRegistersBegin :: Word16
ioRegistersBegin = 0x4000

ioRegistersEnd :: Word16
ioRegistersEnd= 0x4017

cartSpaceBegin :: Word16
cartSpaceBegin = 0x4020

cartSpaceEnd :: Word16
cartSpaceEnd= 0xFFFF

