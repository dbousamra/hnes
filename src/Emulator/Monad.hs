{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Monad (
    -- MonadEmulator(..)
    IOEmulator
  , runIOEmulator
  , load
  , store
) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.ST     (RealWorld, stToIO)
import           Control.Monad.Trans  (MonadIO, lift)
import qualified Data.ByteString      as BS
import           Emulator.Cartridge   (parseCart)
import           Emulator.Nes         as Nes

-- class Monad m => MonadEmulator m where
--   load :: Nes.Address a -> m a
--   store :: Nes.Address a -> a -> m ()

-- newtype IOEmulator a = IOEmulator (ReaderT (Nes RealWorld)  IO a)
--   deriving (Functor, Applicative, Monad, MonadIO)

-- instance MonadEmulator IOEmulator where
--   {-# INLINE load #-}
--   load address = IOEmulator $ do
--     nes <- ask
--     lift $ stToIO $ Nes.read nes address
--   {-# INLINE store #-}
--   store address word = IOEmulator $ do
--     nes <- ask
--     lift $ stToIO $ Nes.write nes address word

newtype IOEmulator a = IOEmulator (ReaderT (Nes RealWorld)  IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

{-# INLINE load #-}
load :: Nes.Address a -> IOEmulator a
load address = IOEmulator $ do
  nes <- ask
  lift $ stToIO $ Nes.read nes address

{-# INLINE store #-}
store :: Nes.Address a -> a -> IOEmulator ()
store address value = IOEmulator $ do
  nes <- ask
  lift $ stToIO $ Nes.write nes address value

runIOEmulator :: BS.ByteString -> IOEmulator a ->  IO a
runIOEmulator bs (IOEmulator reader) = do
  cart <- stToIO $ parseCart bs
  nes <- stToIO $ Nes.new cart
  runReaderT reader nes
