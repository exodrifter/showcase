module Main where

import qualified Control.Concurrent.Async as Async
import qualified Data.List as List
import qualified Data.Text as T
import qualified Dhall
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Showcase.Internal.FileWatch as FileWatch
import qualified Showcase.Types.EntryPoint as EntryPoint
import qualified Showcase.Types.Route as Route
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.DocLayout as DocLayout
import qualified Text.DocTemplates as DocTemplates
import System.FilePath ((</>))

main :: IO ()
main = do
  showcaseContext <- defaultShowcaseContext

  putStrLn "Recompiling HTML..."
  runShowcase showcaseContext $ do
    liftIO (Directory.removePathForcibly (distPath showcaseContext))
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
        Wai.defaultWebAppSettings (distPath showcaseContext)
  Warp.run 8000 (Wai.staticApp waiSettings)

processFileWatchEvent :: ShowcaseContext -> FileWatch.Event -> IO ()
processFileWatchEvent showcaseContext event = do
  runShowcase showcaseContext $ do
    case event of
      FileWatch.FileUpdated _ -> compileAllHTML -- TODO: only compile what changed
      FileWatch.FileRemoved _ -> pure () -- TODO: delete old files

compileAllHTML :: Showcase ()
compileAllHTML = do
  path <- asks entryPointPath

  expr <- liftIO (Dhall.inputExpr (T.pack path))
  case EntryPoint.dhallToEntryPoint expr of
    Left err ->
      putStrLn ("Cannot read " <> path <> "; " <> show err)

    Right ep ->
      traverse_ compileHTML (EntryPoint.routes ep)

compileHTML :: Route.Route -> Showcase ()
compileHTML route = do
  res <- liftIO (DocTemplates.compileTemplateFile (Route.template route))
  case res of
    Left err ->
      putStrLn ("Cannot compile template " <> Route.template route <> "; " <> err)

    Right template -> do
      let
        doc = DocTemplates.renderTemplate template (Route.input route)
        renderedText = DocLayout.render Nothing doc
        uri = Route.uri route

      distFolder <- asks distPath
      let path = distFolder </> uri

      -- TODO: Debouncing?
      putStrLn ("Writing " <> path <> "...")
      liftIO (Directory.createDirectoryIfMissing True (FilePath.takeDirectory path))
      writeFile path renderedText

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

newtype AbsolutePath = AbsolutePath { toFilePath :: FilePath }

-- | Convert a path into an absolute path. (see Directory.makeAbsolute)
makeAbsolute :: FilePath -> IO AbsolutePath
makeAbsolute path = do
  AbsolutePath <$> Directory.makeAbsolute path

-- | Contains variables commonly used throughout all Showcase operations.
data ShowcaseContext =
  ShowcaseContext
    { entryPoint :: AbsolutePath
      -- ^ The absolute path to the file describing all of the routes that
      -- showcase should generate.

    , dist :: AbsolutePath
     -- ^ The absolute path to the directory where the files that showcase
     -- generates should be written to.
    }

entryPointPath :: ShowcaseContext -> FilePath
entryPointPath = toFilePath . entryPoint

dataPath :: ShowcaseContext -> FilePath
dataPath = FilePath.takeDirectory . entryPointPath

distPath :: ShowcaseContext -> FilePath
distPath = toFilePath . dist

-- | Creates the default showcase context, which assumes that the working
-- directory is the same as the current working directory.
defaultShowcaseContext :: IO ShowcaseContext
defaultShowcaseContext = do
  ShowcaseContext
    <$> makeAbsolute "data/index.dhall"
    <*> makeAbsolute "dist/"
