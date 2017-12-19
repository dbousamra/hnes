# hnes

Welcome to hnes. hnes is a NES emulator written in Haskell.

# Building

hnes uses stack. 

It depends on SDL2, so make sure that's installed. To install:

- _Linux_: `apt-get install libsdl2-dev` (on Ubuntu)
  - _MacOS_: `brew install sdl2`
  - _Windows_:
    - Download dev libs from [here](https://www.libsdl.org/download-2.0.php)
    - Modfiy the `SDL2_MORE_INCLUDE_DIR` variable in `CMakeLists.txt` to point
      to the SDL2 dev libs (or just plop them down into `C:\sdl2\`)

To build hnes:

`stack build`

And to run:

`stack exec -- hnes roms/tests/cpu/nestest/nestest.nes`



# Task list
- [x] Basic structure
- [ ] Mappers
  - [x] Loading roms
  - [x] UNROM/Mapper 2
  - [ ] More mappers
- [ ] CPU
  - [x] All official opcodes
  - [ ] All illegal opcodes
  - [x] Nestest passing
  - [ ] Blarggs CPU test roms passing
- [ ] PPU
  - [x] SDL integration
  - [x] Background rendering
  - [x] Scrolling
  - [ ] Sprite rendering
- [x] Controller input handled
- [x] Performance tuning


# Screenshots

Early on in the PPU

<img src="screenshots/nestest-1.png" width="256" height="240"/>

<img src="screenshots/nestest-2.png" width="256" height="240"/>

<img src="screenshots/nestest-3.png" width="256" height="240"/>

<img src="screenshots/nestest-4.png" width="256" height="240"/>

<img src="screenshots/1942-1.png" width="256" height="240"/>

<img src="screenshots/1942-2.png" width="256" height="240"/>

<img src="screenshots/dk-1.png" width="256" height="240"/>

<img src="screenshots/dk-2.png" width="256" height="240"/>

<img src="screenshots/dk-3.png" width="256" height="240"/>

<img src="screenshots/balloon-fight-1.png" width="256" height="240"/>

<img src="screenshots/balloon-fight-2.png" width="256" height="240"/>

<img src="screenshots/contra-1.png" width="256" height="240"/>

<img src="screenshots/contra-2.png" width="256" height="240"/>

<img src="screenshots/megaman-1.png" width="256" height="240"/>

<img src="screenshots/megaman-2.png" width="256" height="240"/>

<img src="screenshots/skydest-1.png" width="256" height="240"/>

<img src="screenshots/slalom-1.png" width="256" height="240"/>

<img src="screenshots/zelda-1.png" width="256" height="240"/>

<img src="screenshots/mario-1.png" width="256" height="240"/>