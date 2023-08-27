module Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Dhall
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Showcase.Internal.FileWatch as FileWatch
import qualified Showcase.Types.Route as Route
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
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
  showcaseContext <- defaultShowcaseContext

  putStrLn "Recompiling HTML..."
  runShowcase showcaseContext $ do
    liftIO (Directory.removePathForcibly (distPath showcaseContext))
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
        Wai.defaultWebAppSettings (distPath showcaseContext)
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

compileAllHTML :: FilePath -> Showcase ()
compileAllHTML folder = do
  listDirResult <- liftIO (Directory.listDirectory folder)
  let absoluteFiles = (folder </>) <$> listDirResult
      filesToCompile = filter isDataFile absoluteFiles
  traverse_ compileHTML filesToCompile

  let absoluteDirs = (folder </>) <$> listDirResult
  dirsResult <- filterM (liftIO . Directory.doesDirectoryExist) absoluteDirs
  traverse_ compileAllHTML dirsResult

compileHTML :: FilePath -> Showcase ()
compileHTML filePath = do
  expr <- liftIO (Dhall.inputExpr (T.pack filePath))
  case Route.dhallToRoute expr of
    Left err ->
      putStrLn ("Cannot read " <> filePath <> "; " <> show err)

    Right item -> do
      res <- liftIO (DocTemplates.compileTemplateFile (Route.template item))
      case res of
        Left err ->
          putStrLn ("Cannot compile template " <> Route.template item <> "; " <> err)

        Right template -> do
          let
            doc = DocTemplates.renderTemplate template (Route.input item)
            renderedText = DocLayout.render Nothing doc
            uri = Route.uri item

          distFolder <- asks distPath
          let path = distFolder </> uri

          -- Remove the old file if it's a different name
          oldFile <- updateDistMap filePath path
          traverse_ removeFile oldFile

          -- TODO: Debouncing?
          putStrLn ("Writing " <> path <> "...")
          liftIO (Directory.createDirectoryIfMissing True (FilePath.takeDirectory path))
          writeFile path renderedText

removeHTML :: FilePath -> Showcase ()
removeHTML filePath = do
  uri <- removeFromDistMap filePath
  traverse_ removeFile uri

removeFile :: FilePath -> Showcase ()
removeFile uri = do
  putStrLn ("Removing " <> uri <> "...")
  liftIO (Directory.removeFile uri)

-- Tries to create the path of the corresponding dist file.
makeDistFilePath :: FilePath -> Showcase (Maybe FilePath)
makeDistFilePath filePath = do
  dataFolder <- asks dataPath
  distFolder <- asks distPath

  if dataFolder `isPrefixOf` filePath
  then
    pure $ do
      relFile <- List.stripPrefix dataFolder filePath
      Just (distFolder </> FilePath.replaceExtension "html" relFile)
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
    { workingPath :: FilePath
      -- ^ The absolute path that all relative paths will be relative to.
    , relDataPath :: FilePath
      -- ^ The relative path to the directory containing the source data.
    , relDistPath :: FilePath
      -- ^ The relative path to the directory containing the generated files.

    , distMap :: MVar (Map FilePath FilePath)
    }

dataPath :: ShowcaseContext -> FilePath
dataPath context =
  workingPath context </> relDataPath context

distPath :: ShowcaseContext -> FilePath
distPath context =
  workingPath context </> relDistPath context

removeFromDistMap :: FilePath -> Showcase (Maybe FilePath)
removeFromDistMap file = do
  distMapMVar <- asks distMap
  liftIO $ MVar.modifyMVar distMapMVar $ \dm ->
    pure (Map.delete file dm, Map.lookup file dm)

updateDistMap :: FilePath -> FilePath -> Showcase (Maybe FilePath)
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
  cwd <- Directory.makeAbsolute "."
  mvar <- newMVar mempty
  pure ShowcaseContext
    { workingPath = cwd
    , relDataPath = "data/"
    , relDistPath = "dist/"
    , distMap = mvar
    }
