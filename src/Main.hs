module Main where

import           Emulator           (run)
import           System.Environment (getArgs)
main :: IO ()
main = do
  fp <- fmap head getArgs
  run fp
