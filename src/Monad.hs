{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad (
  -- * Types
    MonadEmulator(..)
  -- * Functions
  , runIOEmulator
) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.ST       (RealWorld, stToIO)
import           Control.Monad.Trans    (MonadIO, lift)
import           Data.Semigroup         ((<>))
import           Data.Word
import           Nes
import           Util

class Monad m => MonadEmulator m where
  load8 :: Address -> m Word8
  load16 :: Address -> m Word16
  readRegister :: Register -> m Word8
  store :: Address -> Word8 -> m ()
  trace :: String -> m ()

newtype IOEmulator a = IOEmulator (ReaderT (Nes RealWorld)  IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEmulator IOEmulator where
  load8 address = IOEmulator $ do
    mem <- ask
    lift $ stToIO $ Nes.readMemory mem address
  load16 addr @ (Address a) = IOEmulator $ do
    mem <- ask
    lift $ stToIO $ do
      l  <- Nes.readMemory mem addr
      r <- Nes.readMemory mem (Address (a + 1))
      pure $ makeW16 l r
  store address word = IOEmulator $ do
    mem <- ask
    lift $ stToIO $ Nes.writeMemory mem address word
  trace msg = IOEmulator $ do
    mem <- ask
    r <- lift $ stToIO $ Nes.render mem
    liftIO $ print (msg <> " " <> r)

runIOEmulator :: IOEmulator a -> IO a
runIOEmulator (IOEmulator reader) = do
  mem <- stToIO Nes.new
  runReaderT reader mem
