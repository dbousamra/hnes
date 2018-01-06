{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator2.Monad (
    IOEmulator2
  -- , runIOEmulator2
  -- , load
  -- , store
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans    (MonadIO, lift)
import qualified Data.ByteString        as BS

newtype IOEmulator2 b a = IOEmulator2 { unNes :: ReaderT b IO a }
  deriving (Monad, Applicative, Functor, MonadIO)

{-# INLINE load #-}
load :: a -> IOEmulator2 a
load address = IOEmulator2 $ do
  nes <- ask
  lift $ Nes.read nes address

-- {-# INLINE store #-}
-- store :: Nes.Address a -> a -> IOEmulator2 ()
-- store address value = IOEmulator2 $ do
--   nes <- ask
--   lift $ Nes.write nes address value

-- runIOEmulator2 :: BS.ByteString -> IOEmulator2 a ->  IO a
-- runIOEmulator2 bs (IOEmulator2 reader) = do
--   cart <- Cartridge.parse bs
--   nes <- Nes.new cart
--   runReaderT reader nes
