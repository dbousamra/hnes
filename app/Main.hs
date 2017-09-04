module Main where

import           Emulator           (run)
import           System.Environment (getArgs)

main :: IO ()
main = fmap head getArgs >>= run
