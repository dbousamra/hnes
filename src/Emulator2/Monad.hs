{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator2.Monad (
    Emulator(..)
  , run
  , load
  , store
  , modify
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans    (MonadIO, MonadTrans, lift)
import           Data.IORef

newtype Emulator a b = Emulator { unNes :: ReaderT a IO b }
  deriving (Monad, Applicative, Functor, MonadIO)

{-# INLINE run #-}
run :: IO a -> Emulator a b ->  IO b
run new (Emulator reader) = do
  thing <- new
  runReaderT reader thing

{-# INLINE load #-}
load :: (a -> IORef b) -> Emulator a b
load field = Emulator $ do
  thing <- ask
  lift $ readIORef $ field thing

{-# INLINE store #-}
store :: (a -> IORef b) -> b -> Emulator a ()
store field v = modify field (const v)

{-# INLINE modify #-}
modify :: (a -> IORef b) -> (b -> b) -> Emulator a ()
modify field v = Emulator $ do
  thing <- ask
  lift $ modifyIORef' (field thing) v
