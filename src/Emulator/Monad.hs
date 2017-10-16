{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Monad (
    IOEmulator
  , runIOEmulator
  , load
  , store
  , modify
) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans  (MonadIO, lift)
import qualified Data.ByteString      as BS
import qualified Emulator.Cartridge   as Cartridge
import           Emulator.Nes         as Nes

newtype IOEmulator a = IOEmulator { unNes :: ReaderT Nes IO a }
  deriving (Monad, Applicative, Functor, MonadIO)

load :: Nes.Address a -> IOEmulator a
load address = IOEmulator $ do
  nes <- ask
  lift $ Nes.read nes address

store :: Nes.Address a -> a -> IOEmulator ()
store address value = IOEmulator $ do
  nes <- ask
  lift $ Nes.write nes address value

modify :: Address a -> (a -> a) -> IOEmulator ()
modify addr f = do
  av <- load addr
  store addr (f av)

runIOEmulator :: BS.ByteString -> IOEmulator a ->  IO a
runIOEmulator bs (IOEmulator reader) = do
  cart <- Cartridge.parse bs
  nes <- Nes.new cart
  runReaderT reader nes
