{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.JSON as DhallJSON
import qualified Dhall.Map as Map
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FSNotify as FSNotify
import qualified Text.DocLayout as DocLayout
import qualified Text.DocTemplates as DocTemplates
import System.FilePath ((</>))

isDataFile :: FilePath -> Bool
isDataFile filePath =
     FilePath.takeBaseName filePath /= "website"
  && FilePath.takeExtension filePath == ".dhall"

lookupDataFolder :: IO FilePath
lookupDataFolder = Directory.makeAbsolute "data/"

lookupDistFolder :: IO FilePath
lookupDistFolder = Directory.makeAbsolute "dist/"

main :: IO ()
main = do
  dataFolder <- lookupDataFolder
  distFolder <- lookupDistFolder

  putStrLn "Setting up FSNotify..."
  FSNotify.withManager $ \manager -> do
    _ <- FSNotify.watchDir
      manager
      dataFolder
      (const True)
      processFSNotifyEvent

    putStrLn "Recompiling HTML..."
    Directory.removePathForcibly distFolder
    compileAllHTML dataFolder

    putStrLn "Running webserver on 8000..."
    Warp.run 8000 (Wai.staticApp (Wai.defaultWebAppSettings distFolder))

processFSNotifyEvent :: FSNotify.Event -> IO ()
processFSNotifyEvent event = do
  let processFile filePath
        | isDataFile filePath = compileHTML filePath
        | otherwise = compileAllHTML filePath

  case event of
    FSNotify.Added { FSNotify.eventPath = filePath } -> processFile filePath
    FSNotify.Modified { FSNotify.eventPath = filePath } -> processFile filePath
    FSNotify.Removed { FSNotify.eventPath = filePath } -> removeHTML filePath

    FSNotify.ModifiedAttributes {} -> mempty
    FSNotify.WatchedDirectoryRemoved {} -> mempty
    FSNotify.CloseWrite {} -> mempty

    FSNotify.Unknown { FSNotify.eventString = eventString } ->
      putStrLn ("Unknown event: " <> eventString)

distHtmlFileName :: FilePath -> FilePath
distHtmlFileName filePath =
  let fileName = FilePath.takeBaseName filePath
  in  fileName <> ".html"

compileAllHTML :: FilePath -> IO ()
compileAllHTML filePath = do
  let directory = FilePath.takeDirectory filePath
  relativePaths <- Directory.listDirectory directory

  let absolutePaths = (directory </>) <$> relativePaths
      filesToCompile = filter isDataFile absolutePaths
  traverse_ compileHTML filesToCompile

compileHTML :: FilePath -> IO ()
compileHTML filePath = do
  distFolder <- lookupDistFolder
  let htmlPath = distFolder <> distHtmlFileName filePath

  expr <- Dhall.inputExpr (T.pack filePath)
  case dhallToItem expr of
    Left err ->
      putStrLn ("Cannot read " <> filePath <> "; " <> show err)

    Right item -> do
      res <- DocTemplates.compileTemplateFile (templatePath item)
      case res of
        Left err ->
          putStrLn ("Cannot compile template " <> templatePath item <> "; " <> err)

        Right template -> do
          let
            doc = DocTemplates.renderTemplate template (metadata item)
            renderedText = DocLayout.render Nothing doc

          -- TODO: Debouncing?
          putStrLn ("Writing " <> htmlPath <> "...")
          Directory.createDirectoryIfMissing True "dist"
          writeFile htmlPath renderedText

removeHTML :: FilePath -> IO ()
removeHTML filePath = do
  distFolder <- lookupDistFolder
  let htmlPath = distFolder <> distHtmlFileName filePath

  putStrLn ("Removing " <> htmlPath <> "...")
  Directory.removeFile htmlPath

--------------------------------------------------------------------------------
-- Dhall Types
--------------------------------------------------------------------------------

data Item =
  Item
    { templatePath :: FilePath
    , metadata :: Aeson.Value
    } deriving (Generic, Aeson.FromJSON)

dhallToString :: Core.Expr Void Void -> Either DhallJSON.CompileError String
dhallToString expr =
  case expr of
    Core.TextLit (Core.Chunks [] t) -> pure (T.unpack t)
    _ -> Left (DhallJSON.Unsupported expr)

dhallToItem :: Core.Expr s Void -> Either DhallJSON.CompileError Item
dhallToItem expr =
  let
    e = Core.alphaNormalize (Core.normalize expr)
  in
    case e of
      Core.RecordLit a ->
        case (Map.lookup "template" a, Map.lookup "data" a) of
          (Just t, Just d) -> do
            p <- dhallToString (Core.recordFieldValue t)
            v <- DhallJSON.dhallToJSON (Core.recordFieldValue d)
            pure Item { templatePath = p, metadata = v }
          _ ->
            Left (DhallJSON.Unsupported e)
      _ ->
        Left (DhallJSON.Unsupported e)
