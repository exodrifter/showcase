module Main where

import qualified Data.Text as T
import qualified Dhall
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FSNotify as FSNotify
import Data.Foldable (traverse_)
import System.FilePath ((</>))

main :: IO ()
main = do
  putStrLn "Setting up FSNotify..."
  FSNotify.withManager $ \manager -> do
    _ <- FSNotify.watchDir
      manager
      "data/"
      (const True)
      processFSNotifyEvent

    putStrLn "Running webserver on 8000..."
    Warp.run 8000 (Wai.staticApp (Wai.defaultWebAppSettings "dist"))

processFSNotifyEvent :: FSNotify.Event -> IO ()
processFSNotifyEvent event = do
  let processFile filePath =
        if isWebsite filePath
          then compileAllHTML filePath
          else compileHTML filePath

  case event of
    FSNotify.Added { FSNotify.eventPath = filePath } -> processFile filePath
    FSNotify.Modified { FSNotify.eventPath = filePath } -> processFile filePath
    FSNotify.Removed { FSNotify.eventPath = filePath } -> removeHTML filePath

    FSNotify.ModifiedAttributes {} -> mempty
    FSNotify.WatchedDirectoryRemoved {} -> mempty
    FSNotify.CloseWrite {} -> mempty

    FSNotify.Unknown { FSNotify.eventString = eventString } ->
      putStrLn ("Unknown event: " <> eventString)

isWebsite :: FilePath -> Bool
isWebsite filePath = FilePath.takeBaseName filePath == "website"

compileAllHTML :: FilePath -> IO ()
compileAllHTML filePath = do
  let directory = FilePath.takeDirectory filePath
  relativePaths <- Directory.listDirectory directory

  let absolutePaths = (directory </>) <$> relativePaths
      filesToCompile = filter (/= filePath) absolutePaths
  traverse_ compileHTML filesToCompile

distHtmlPath :: FilePath -> FilePath
distHtmlPath filePath =
  let fileName = FilePath.takeBaseName filePath
  in  "dist/" <> fileName <> ".html"

compileHTML :: FilePath -> IO ()
compileHTML filePath = do
  let htmlPath = distHtmlPath filePath

  -- TODO: Debouncing?
  putStrLn ("Writing " <> htmlPath <> "...")
  html <- Dhall.input Dhall.auto ("(./data/website.dhall).schemaToHTML " <> T.pack filePath)
  Directory.createDirectoryIfMissing True "dist"
  writeFile htmlPath html

removeHTML :: FilePath -> IO ()
removeHTML filePath = do
  let htmlPath = distHtmlPath filePath
  putStrLn ("Removing " <> htmlPath <> "...")
  Directory.removeFile htmlPath
