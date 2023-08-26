-- | Module responsible for watching for file changes.
module Showcase.Internal.FileWatch
  ( Event (..)
  , directory
  ) where

import qualified System.FSNotify as FSNotify
import Control.Concurrent (threadDelay)
import qualified Path
import Path (Path, Abs, Dir, File)

data Event =
    FileUpdated (Path Abs File)
  | FileRemoved (Path Abs File)

directory :: Path Abs Dir -> (Event -> IO ()) -> IO ()
directory dir callback =
  FSNotify.withManager $ \manager -> do
    _ <- FSNotify.watchDir
      manager
      (Path.fromAbsDir dir)
      (const True)
      (processFSNotifyEvent callback)

    -- Sleep forever
    forever (threadDelay 1000000)

processFSNotifyEvent :: (Event -> IO ()) -> FSNotify.Event -> IO ()
processFSNotifyEvent callback event = do
  case event of
    FSNotify.Added { FSNotify.eventPath = path } ->
      case Path.parseAbsFile path of
        Just absFilePath ->
          callback (FileUpdated absFilePath)
        Nothing ->
          pure ()

    FSNotify.Modified { FSNotify.eventPath = path } ->
      case Path.parseAbsFile path of
        Just absFilePath ->
          callback (FileUpdated absFilePath)
        Nothing ->
          pure ()

    FSNotify.Removed { FSNotify.eventPath = path } -> do
      case Path.parseAbsFile path of
        Just absFilePath ->
          callback (FileRemoved absFilePath)
        Nothing ->
          pure ()

    FSNotify.ModifiedAttributes {} ->
      pure ()
    FSNotify.WatchedDirectoryRemoved {} ->
      pure ()
    FSNotify.CloseWrite {} ->
      pure ()
    FSNotify.Unknown {} ->
      pure ()
