# hnes

[![Build Status](https://travis-ci.org/dbousamra/hnes.svg?branch=master)](https://travis-ci.org/dbousamra/hnes)

Welcome to hnes. hnes is a NES emulator written in Haskell.

## Screenshots

<p float="left">
  <img src="screenshots/nestest-4.png" width="200" />
  <img src="screenshots/mario-1.png" width="200" />
  <img src="screenshots/mario-2.png" width="200" />
  <img src="screenshots/megaman-3.png" width="200" />
  <img src="screenshots/contra-1.png" width="200" />
  <img src="screenshots/contra-2.png" width="200" />
</p>

## Controls

| Nintendo              | Emulator    |
| --------------------- | ----------- |
| Up, Down, Left, Right | Arrow Keys  |
| Start                 | Enter       |
| Select                | Space       |
| A                     | Z           |
| B                     | X           |

## Building

hnes uses stack.

It depends on SDL2, so make sure that's installed. To install:

- _Linux_: `apt-get install libsdl2-dev` (on Ubuntu)
- _MacOS_: `brew install sdl2`
- _Windows_: You are on your own.

To build hnes:

`stack build`

And to run:

`stack exec -- hnes roms/tests/cpu/nestest/nestest.nes`

## Task list

- [x] Basic structure
- [ ] Mappers
  - [x] Loading roms
  - [x] Mappers
    - [x] Mapper 2
    - [x] Mapper 3
    - [x] Mapper 7
- [ ] CPU
  - [x] All official opcodes
  - [ ] All illegal opcodes
  - [x] Nestest passing
  - [x] Blarggs CPU test roms passing
- [ ] PPU
  - [x] SDL integration
  - [x] Background rendering
  - [x] Scrolling
  - [x] Sprite rendering
  - [ ] VBlank timing for Battletoads edge case
- [x] Controller input handled
- [x] Performance tuning
