{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Monad (
    IOEmulator
  , runIOEmulator
  , load
  , store
  , modify
  , trace
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans    (MonadIO, lift)
import qualified Data.ByteString        as BS
import qualified Emulator.Cartridge     as Cartridge
import           Emulator.Nes           as Nes

newtype IOEmulator a = IOEmulator { unNes :: ReaderT Nes IO a }
  deriving (Monad, Applicative, Functor, MonadIO)

{-# INLINE load #-}
load :: Nes.Address a -> IOEmulator a
load address = IOEmulator $ do
  nes <- ask
  lift $ Nes.read nes address

{-# INLINE store #-}
store :: Nes.Address a -> a -> IOEmulator ()
store address value = IOEmulator $ do
  nes <- ask
  lift $ Nes.write nes address value

{-# INLINE modify #-}
modify :: Address a -> (a -> a) -> IOEmulator ()
modify addr f = do
  av <- load addr
  store addr (f av)

trace :: String -> IOEmulator ()
trace = liftIO . putStrLn

runIOEmulator :: BS.ByteString -> IOEmulator a ->  IO a
runIOEmulator bs (IOEmulator reader) = do
  cart <- Cartridge.parse bs
  nes <- Nes.new cart
  runReaderT reader nes
