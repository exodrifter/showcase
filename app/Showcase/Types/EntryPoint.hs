module Showcase.Types.EntryPoint
  ( EntryPoint
  , routes
  , dhallToEntryPoint
  ) where

import qualified Dhall.Core as Core
import qualified Dhall.Map as DhallMap
import qualified Showcase.Types.Route as Route

-- | Represents a route in the website.
data EntryPoint =
  EntryPoint
    { routes :: [Route.Route]
    -- ^ The routes to generate for the website.
    }

dhallToEntryPoint :: Core.Expr s Void -> Either Text EntryPoint
dhallToEntryPoint expr =
  let
    e = Core.alphaNormalize (Core.normalize expr)
  in
    case e of
      Core.RecordLit a ->
        EntryPoint
          <$> (dhallToRoutes =<< dhallLookup "routes" a)
      _ ->
        Left "Unsupported type"

dhallToRoutes :: Core.Expr s Void -> Either Text [Route.Route]
dhallToRoutes expr =
  let
    e = Core.alphaNormalize (Core.normalize expr)
  in
    case e of
      Core.RecordLit a -> do
        let values = Core.recordFieldValue <$> DhallMap.elems a
        results <- traverse Route.dhallToRoutes values
        pure (concat results)
      _ ->
        Left "Unsupported type"

dhallLookup :: Text -> DhallMap.Map Text (Core.RecordField s a) -> Either Text (Core.Expr s a)
dhallLookup key m =
  case DhallMap.lookup key m of
    Just a -> Right (Core.recordFieldValue a)
    Nothing -> Left ("Cannot find key " <> key)
