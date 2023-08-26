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
  showcaseContext <- defaultShowcaseContext

  putStrLn "Setting up FSNotify..."
  FSNotify.withManager $ \manager -> do
    _ <- FSNotify.watchDir
      manager
      (dataPath showcaseContext)
      (const True)
      (processFSNotifyEvent showcaseContext)

    putStrLn "Recompiling HTML..."
    runShowcase showcaseContext $ do
      liftIO (Directory.removePathForcibly (distPath showcaseContext))
      compileAllHTML (dataPath showcaseContext)

    putStrLn "Running webserver on 8000..."
    let waiSettings =
          Wai.defaultWebAppSettings (distPath showcaseContext)
    Warp.run 8000 (Wai.staticApp waiSettings)

processFSNotifyEvent :: ShowcaseContext -> FSNotify.Event -> IO ()
processFSNotifyEvent showcaseContext event = do
  let processFile filePath
        | isDataFile filePath = compileHTML filePath
        | otherwise = compileAllHTML filePath

  runShowcase showcaseContext $ do
    case event of
      FSNotify.Added { FSNotify.eventPath = filePath } ->
        processFile filePath
      FSNotify.Modified { FSNotify.eventPath = filePath } ->
        processFile filePath
      FSNotify.Removed { FSNotify.eventPath = filePath } ->
        removeHTML filePath

      FSNotify.ModifiedAttributes {} ->
        pure ()
      FSNotify.WatchedDirectoryRemoved {} ->
        pure ()
      FSNotify.CloseWrite {} ->
        pure ()

      FSNotify.Unknown { FSNotify.eventString = eventString } ->
        putStrLn ("Unknown event: " <> eventString)

distHtmlFileName :: FilePath -> FilePath
distHtmlFileName filePath =
  let fileName = FilePath.takeBaseName filePath
  in  fileName <> ".html"

compileAllHTML :: FilePath -> Showcase ()
compileAllHTML filePath = do
  let directory = FilePath.takeDirectory filePath
  relativePaths <- liftIO (Directory.listDirectory directory)

  let absolutePaths = (directory </>) <$> relativePaths
      filesToCompile = filter isDataFile absolutePaths
  traverse_ compileHTML filesToCompile

compileHTML :: FilePath -> Showcase ()
compileHTML filePath = do
  distFolder <- asks distPath
  let htmlPath = distFolder <> distHtmlFileName filePath

  expr <- liftIO (Dhall.inputExpr (T.pack ("." </> filePath)))
  case dhallToItem expr of
    Left err ->
      putStrLn ("Cannot read " <> filePath <> "; " <> show err)

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
          putStrLn ("Writing " <> htmlPath <> "...")
          liftIO (Directory.createDirectoryIfMissing True "dist")
          writeFile htmlPath renderedText

removeHTML :: FilePath -> Showcase ()
removeHTML filePath = do
  distFolder <- asks distPath
  let htmlPath = distFolder <> distHtmlFileName filePath

  putStrLn ("Removing " <> htmlPath <> "...")
  liftIO (Directory.removeFile htmlPath)

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
    , dataPath :: FilePath
      -- ^ The relative path to the directory containing the source data.
    , distPath :: FilePath
      -- ^ The relative path to the directory containing the generated files.
    }

-- | Creates the default showcase context, which assumes that the working
-- directory is the same as the current working directory.
defaultShowcaseContext :: IO ShowcaseContext
defaultShowcaseContext = do
  cwd <- Directory.makeAbsolute "."
  pure ShowcaseContext
    { workingPath = cwd
    , dataPath = "data/"
    , distPath = "dist/"
    }

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
