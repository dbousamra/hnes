module Log (
  -- * Functions
    render
) where

import           Monad
import           Nes   (Address (..), Flag (..))
import           Util

render :: MonadEmulator m => m String
render = do
  pcv <- load Pc
  spv <- load Sp
  xv  <- load X
  yv  <- load Y
  pure $ "PC: " ++ (prettifyWord16 pcv) ++ " " ++
         "SP: " ++ (prettifyWord8 spv) ++ " " ++
         "X: " ++ (prettifyWord8 xv) ++ " " ++
         "Y: " ++ (prettifyWord8 xv) ++ " "
