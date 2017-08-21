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
  load :: Address a -> m a
  store :: Address a -> a -> m ()
  trace :: String -> m ()

newtype IOEmulator a = IOEmulator (ReaderT (Nes RealWorld)  IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEmulator IOEmulator where
  load address = IOEmulator $ do
    mem <- ask
    lift $ stToIO $ Nes.load mem address
  store address word = IOEmulator $ do
    mem <- ask
    lift $ stToIO $ Nes.store mem address word
  trace msg = IOEmulator $ do
    mem <- ask
    liftIO $ print (msg <> " " <> "")

runIOEmulator :: IOEmulator a -> IO a
runIOEmulator (IOEmulator reader) = do
  mem <- stToIO Nes.new
  runReaderT reader mem
