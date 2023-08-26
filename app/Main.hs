module Main where

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.JSON as DhallJSON
import qualified Dhall.Map as Map
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
    compileAllHTML

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
        | otherwise = compileAllHTML

  runShowcase showcaseContext $ do
    case event of
      FileWatch.FileUpdated file -> processFile file
      FileWatch.FileRemoved file -> removeHTML file

compileAllHTML :: Showcase ()
compileAllHTML = do
  dataFolder <- asks dataPath
  listDirResult <- liftIO (Directory.listDirectory (Path.fromAbsDir dataFolder))
  let relativePaths = concatMap Path.parseRelFile listDirResult
  let absolutePaths = (dataFolder </>) <$> relativePaths
      filesToCompile = filter isDataFile absolutePaths
  traverse_ compileHTML filesToCompile

compileHTML :: Path Abs File -> Showcase ()
compileHTML filePath = do
  mDistFile <- makeDistFilePath filePath
  case mDistFile of
    Nothing ->
      pure ()

    Just distFile -> do
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
              putStrLn ("Writing " <> Path.fromAbsFile distFile <> "...")
              liftIO (Directory.createDirectoryIfMissing True "dist")
              writeFile (Path.fromAbsFile distFile) renderedText

removeHTML :: Path Abs File -> Showcase ()
removeHTML filePath = do
  mDistFile <- makeDistFilePath filePath
  case mDistFile of
    Nothing -> do
      pure ()

    Just distFile -> do
      putStrLn ("Removing " <> Path.fromAbsFile distFile <> "...")
      liftIO (Directory.removeFile (Path.fromAbsFile distFile))

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
    }

dataPath :: ShowcaseContext -> Path Abs Dir
dataPath context =
  workingPath context </> relDataPath context

distPath :: ShowcaseContext -> Path Abs Dir
distPath context =
  workingPath context </> relDistPath context

-- | Creates the default showcase context, which assumes that the working
-- directory is the same as the current working directory.
defaultShowcaseContext :: IO ShowcaseContext
defaultShowcaseContext = do
  ShowcaseContext
    <$> (Path.parseAbsDir =<< Directory.makeAbsolute ".")
    <*> Path.parseRelDir "data"
    <*> Path.parseRelDir "dist"

--------------------------------------------------------------------------------
-- Dhall Types
--------------------------------------------------------------------------------

data Item =
  Item
    { templatePath :: FilePath
    , metadata :: Aeson.Value
    }

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
