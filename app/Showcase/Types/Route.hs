module Showcase.Types.Route
  ( Route
  , template
  , uri
  , input
  , dhallToRoute
  , dhallToRoutes
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Dhall.Core as Core
import qualified Dhall.Map as DhallMap
import qualified Dhall.JSON as DhallJSON

-- | Represents a route in the website.
data Route =
  Route
    { template :: FilePath
    -- ^ The template to use for the route.
    , uri :: String
    -- ^ The relative location of this route in the website.
    , input :: Aeson.Value
    -- ^ The input data for the route's template.
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

dhallToRoute :: Core.Expr s Void -> Either Text Route
dhallToRoute expr =
  let
    e = Core.alphaNormalize (Core.normalize expr)
  in
    case e of
      Core.RecordLit a ->
        Route
          <$> (dhallToString =<< dhallLookup "template" a)
          <*> (dhallToString =<< dhallLookup "uri" a)
          <*> (dhallToJSON =<< dhallLookup "input" a)
      _ ->
        Left "Unsupported type"

dhallToRoutes :: Core.Expr s Void -> Either Text [Route]
dhallToRoutes expr =
  let
    e = Core.alphaNormalize (Core.normalize expr)
  in
    case e of
      Core.ListLit _ a -> do
        toList <$> traverse dhallToRoute a
      _ ->
        Left "Unsupported type"

dhallLookup :: Text -> DhallMap.Map Text (Core.RecordField s a) -> Either Text (Core.Expr s a)
dhallLookup key m =
  case DhallMap.lookup key m of
    Just a -> Right (Core.recordFieldValue a)
    Nothing -> Left ("Cannot find key " <> key)
