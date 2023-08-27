-- | Module responsible for watching for file changes.
module Showcase.Internal.FileWatch
  ( Event (..)
  , directory
  ) where

import qualified System.FSNotify as FSNotify
import qualified Control.Concurrent as Concurrent

data Event =
    FileUpdated FilePath
  | FileRemoved FilePath

directory :: FilePath -> (Event -> IO ()) -> IO ()
directory dir callback =
  FSNotify.withManager $ \manager -> do
    _ <- FSNotify.watchTree
      manager
      dir
      (const True)
      (processFSNotifyEvent callback)

    -- Sleep forever
    forever (Concurrent.threadDelay 1000000)

processFSNotifyEvent :: (Event -> IO ()) -> FSNotify.Event -> IO ()
processFSNotifyEvent callback event = do
  case event of
    FSNotify.Added { FSNotify.eventPath = path } ->
      callback (FileUpdated path)
    FSNotify.Modified { FSNotify.eventPath = path } ->
      callback (FileUpdated path)
    FSNotify.Removed { FSNotify.eventPath = path } ->
      callback (FileRemoved path)

    FSNotify.ModifiedAttributes {} ->
      pure ()
    FSNotify.WatchedDirectoryRemoved {} ->
      pure ()
    FSNotify.CloseWrite {} ->
      pure ()
    FSNotify.Unknown {} ->
      pure ()
