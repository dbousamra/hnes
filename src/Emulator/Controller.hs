module Emulator.Controller (
  Key(..)
) where

data Key
  = A
  | B
  | Up
  | Down
  | Left
  | Right
  | Reset
  | Select
  | Start
  deriving (Show, Eq)
