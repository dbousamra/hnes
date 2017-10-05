{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Monad (
    IOEmulator
  , runIOEmulator
  , load
  , store
) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans  (MonadIO, lift)
import qualified Data.ByteString      as BS
import           Emulator.Cartridge   (parseCart)
import           Emulator.Nes         as Nes

newtype IOEmulator a = IOEmulator (ReaderT Nes IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

load :: Nes.Address a -> IOEmulator a
load address = IOEmulator $ do
  nes <- ask
  lift $ Nes.read nes address

store :: Nes.Address a -> a -> IOEmulator ()
store address value = IOEmulator $ do
  nes <- ask
  lift $ Nes.write nes address value

runIOEmulator :: BS.ByteString -> IOEmulator a ->  IO a
runIOEmulator bs (IOEmulator reader) = do
  cart <- parseCart bs
  nes <- Nes.new cart
  runReaderT reader nes
