module Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.JSON as DhallJSON
import qualified Dhall.Map as DhallMap
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Path
import qualified Showcase.Internal.FileWatch as FileWatch
import qualified System.Directory as Directory
import qualified Text.DocLayout as DocLayout
import qualified Text.DocTemplates as DocTemplates
import Path ((</>), Path, Abs, Dir, File, Rel)

isDataFile :: Path b File -> Bool
isDataFile filePath =
     Path.fromRelFile (Path.filename filePath) /= "website.dhall"
  && Path.fileExtension filePath == Just ".dhall"

main :: IO ()
main = do
  showcaseContext <- defaultShowcaseContext

  putStrLn "Recompiling HTML..."
  runShowcase showcaseContext $ do
    liftIO (Directory.removePathForcibly
      (Path.fromAbsDir (distPath showcaseContext)))
    compileAllHTML =<< asks dataPath

  Async.concurrently_
    ( FileWatch.directory
        (dataPath showcaseContext)
        (processFileWatchEvent showcaseContext)
    )
    (runServer showcaseContext)

runServer :: ShowcaseContext -> IO ()
runServer showcaseContext = do
  putStrLn "Running webserver on 8000..."
  let waiSettings =
        Wai.defaultWebAppSettings
          (Path.fromAbsDir (distPath showcaseContext))
  Warp.run 8000 (Wai.staticApp waiSettings)

processFileWatchEvent :: ShowcaseContext -> FileWatch.Event -> IO ()
processFileWatchEvent showcaseContext event = do
  let processFile filePath
        | isDataFile filePath = compileHTML filePath
        | otherwise = compileAllHTML =<< asks dataPath

  runShowcase showcaseContext $ do
    case event of
      FileWatch.FileUpdated file -> processFile file
      FileWatch.FileRemoved file -> removeHTML file

compileAllHTML :: Path Abs Dir -> Showcase ()
compileAllHTML folder = do
  listDirResult <- liftIO (Directory.listDirectory (Path.fromAbsDir folder))
  let relativeFiles = concatMap Path.parseRelFile listDirResult
      absoluteFiles = (folder </>) <$> relativeFiles
      filesToCompile = filter isDataFile absoluteFiles
  traverse_ compileHTML filesToCompile

  let relativeDirs = concatMap Path.parseRelDir listDirResult
      absoluteDirs = (folder </>) <$> relativeDirs
  dirsResult <- filterM (liftIO . Directory.doesDirectoryExist) (Path.fromAbsDir <$> absoluteDirs)
  traverse_ compileAllHTML (concatMap Path.parseAbsDir dirsResult)

compileHTML :: Path Abs File -> Showcase ()
compileHTML filePath = do
  expr <- liftIO (Dhall.inputExpr (T.pack (Path.fromAbsFile filePath)))
  case dhallToItem expr of
    Left err ->
      putStrLn ("Cannot read " <> Path.fromAbsFile filePath <> "; " <> show err)

    Right item -> do
      res <- liftIO (DocTemplates.compileTemplateFile (templatePath item))
      case res of
        Left err ->
          putStrLn ("Cannot compile template " <> templatePath item <> "; " <> err)

        Right template -> do
          let
            doc = DocTemplates.renderTemplate template (metadata item)
            renderedText = DocLayout.render Nothing doc

          -- TODO: Debouncing?
          case Path.parseRelFile (distURI item) of
            Nothing ->
              putStrLn "Cannot convert distURI to a path"
            Just relPath -> do
              distFolder <- asks distPath
              let path = distFolder </> relPath

              -- Remove the old file if it's a different name
              oldFile <- updateDistMap filePath path
              traverse_ removeFile oldFile

              putStrLn ("Writing " <> Path.fromAbsFile path <> "...")
              liftIO (Directory.createDirectoryIfMissing True (Path.fromAbsDir (Path.parent path)))
              writeFile (Path.fromAbsFile path) renderedText

removeHTML :: Path Abs File -> Showcase ()
removeHTML filePath = do
  uri <- removeFromDistMap filePath
  traverse_ removeFile uri

removeFile :: Path Abs File -> Showcase ()
removeFile uri = do
  putStrLn ("Removing " <> Path.fromAbsFile uri <> "...")
  liftIO (Directory.removeFile (Path.fromAbsFile uri))

-- Tries to create the path of the corresponding dist file.
makeDistFilePath :: Path Abs File -> Showcase (Maybe (Path Abs File))
makeDistFilePath filePath = do
  dataFolder <- asks dataPath
  distFolder <- asks distPath

  if dataFolder `Path.isProperPrefixOf` filePath
  then
    pure $ do
      relFile <- Path.stripProperPrefix dataFolder filePath
      distFile <- Path.replaceExtension ".html" relFile
      Just (distFolder </> distFile)
  else
    pure Nothing


--------------------------------------------------------------------------------
-- Monad Stack
--------------------------------------------------------------------------------

-- | The monad used for Showcase operations.
newtype Showcase a =
  Showcase (ReaderT ShowcaseContext IO a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader ShowcaseContext
    )

-- | Runs a Showcase operation.
runShowcase :: ShowcaseContext -> Showcase a -> IO a
runShowcase context (Showcase showcase) = do
  runReaderT showcase context

-- | Contains variables commonly used throughout all Showcase operations.
data ShowcaseContext =
  ShowcaseContext
    { workingPath :: Path Abs Dir
      -- ^ The absolute path that all relative paths will be relative to.
    , relDataPath :: Path Rel Dir
      -- ^ The relative path to the directory containing the source data.
    , relDistPath :: Path Rel Dir
      -- ^ The relative path to the directory containing the generated files.

    , distMap :: MVar (Map (Path Abs File) (Path Abs File))
    }

dataPath :: ShowcaseContext -> Path Abs Dir
dataPath context =
  workingPath context </> relDataPath context

distPath :: ShowcaseContext -> Path Abs Dir
distPath context =
  workingPath context </> relDistPath context

removeFromDistMap :: Path Abs File -> Showcase (Maybe (Path Abs File))
removeFromDistMap file = do
  distMapMVar <- asks distMap
  liftIO $ MVar.modifyMVar distMapMVar $ \dm ->
    pure (Map.delete file dm, Map.lookup file dm)

updateDistMap :: Path Abs File -> Path Abs File -> Showcase (Maybe (Path Abs File))
updateDistMap file uri = do
  distMapMVar <- asks distMap
  liftIO $ MVar.modifyMVar distMapMVar $ \dm ->
    let
      oldPath =
        if Map.lookup file dm == Just uri
        then Nothing
        else Map.lookup file dm
    in
      pure (Map.insert file uri dm, oldPath)

-- | Creates the default showcase context, which assumes that the working
-- directory is the same as the current working directory.
defaultShowcaseContext :: IO ShowcaseContext
defaultShowcaseContext = do
  ShowcaseContext
    <$> (Path.parseAbsDir =<< Directory.makeAbsolute ".")
    <*> Path.parseRelDir "data"
    <*> Path.parseRelDir "dist"
    <*> newMVar mempty

--------------------------------------------------------------------------------
-- Dhall Types
--------------------------------------------------------------------------------

data Item =
  Item
    { templatePath :: FilePath
    , distURI :: String
    , metadata :: Aeson.Value
    }

dhallToString :: Core.Expr Void Void -> Either Text String
dhallToString expr =
  case expr of
    Core.TextLit (Core.Chunks [] t) -> pure (T.unpack t)
    _ -> Left "Unexpected type; expected string"

dhallToJSON :: Core.Expr Void Void -> Either Text Aeson.Value
dhallToJSON expr =
  case DhallJSON.dhallToJSON expr of
    Right a -> pure a
    Left err -> Left (T.pack (show err))

dhallToItem :: Core.Expr s Void -> Either Text Item
dhallToItem expr =
  let
    e = Core.alphaNormalize (Core.normalize expr)
  in
    case e of
      Core.RecordLit a ->
        Item
          <$> (dhallToString =<< dhallLookup "template" a)
          <*> (dhallToString =<< dhallLookup "distUri" a)
          <*> (dhallToJSON =<< dhallLookup "data" a)
      _ ->
        Left "Unsupported type"

dhallLookup :: Text -> DhallMap.Map Text (Core.RecordField s a) -> Either Text (Core.Expr s a)
dhallLookup key m =
  case DhallMap.lookup key m of
    Just a -> Right (Core.recordFieldValue a)
    Nothing -> Left ("Cannot find key " <> key)
