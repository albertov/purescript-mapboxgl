module Mapbox.TileJSON where

import Prelude

import Data.Array (fromFoldable, uncons)
import Data.Foreign (ForeignError(..), fail, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.String (Pattern(..), split)
import Mapbox.Common (URI, Bounds, LonLatZoom, Zoom, error)

data SemVersion = SemVersion Int Int Int

instance encodeSemVersion :: Encode SemVersion where
  encode = toForeign <<< showVersion

instance decodeSemVersion :: Decode SemVersion where
  decode = maybe (fail (ForeignError "Invalid version")) pure
       <<< parseSemVersion
       <=< decode

showVersion :: SemVersion -> String
showVersion (SemVersion major minor patch) =
    show major <> "." <>  show minor <> "." <> show patch

parseSemVersion :: String -> Maybe SemVersion
parseSemVersion s
  | [maj,min,ptch] <- split (Pattern ".") s
  = SemVersion <$> fromString maj <*> fromString min <*> fromString ptch
parseSemVersion _ = Nothing

-- | Implements TileJSON spec 2.2.0
-- | See: https://github.com/mapbox/tilejson-spec/tree/master/2.2.0

newtype TileJSON = TileJSON
    { name        :: Maybe String
    , description :: Maybe String
    , version     :: Maybe SemVersion
    , attribution :: Maybe String
    , template    :: Maybe MustacheTemplate
    , legend      :: Maybe String
    , scheme      :: Maybe TileScheme
    , tiles       :: NonEmpty Array URI
    , grids       :: Array URI
    , data        :: Array URI
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , bounds      :: Maybe Bounds
    , center      :: Maybe LonLatZoom
    }


instance encodeTileJSON :: Encode TileJSON where
  encode (TileJSON o) = toForeign
    { name        : encode (NullOrUndefined o.name)
    , description : encode (NullOrUndefined o.description)
    , version     : encode (NullOrUndefined o.version)
    , attribution : encode (NullOrUndefined o.attribution)
    , template    : encode (NullOrUndefined o.template)
    , legend      : encode (NullOrUndefined o.legend)
    , scheme      : encode (NullOrUndefined o.scheme)
    , tiles       : encode (fromFoldable o.tiles)
    , grids       : encode o.grids
    , data        : encode o.data
    , minzoom     : encode (NullOrUndefined o.minzoom)
    , maxzoom     : encode (NullOrUndefined o.maxzoom)
    , bounds      : encode (NullOrUndefined o.bounds)
    , center      : encode (NullOrUndefined o.center)
    }

instance decodeTileJSON :: Decode TileJSON where
  decode o = readStrMap o *> do
    let s = unsafeFromForeign o
    name <- unNullOrUndefined <$> decode s.name
    description <- unNullOrUndefined <$> decode s.description
    version <- unNullOrUndefined <$> decode s.version
    attribution <- unNullOrUndefined <$> decode s.attribution
    template <- unNullOrUndefined <$> decode s.template
    legend <- unNullOrUndefined <$> decode s.legend
    scheme <- unNullOrUndefined <$> decode s.scheme
    tilesArr <- maybe (error "tiles") pure
            =<< unNullOrUndefined <$> decode s.tiles
    tiles <- case uncons tilesArr of
      Just {head,tail} -> pure (NonEmpty head tail)
      Nothing -> fail (ForeignError "tiles array must have at least one element")
    grids <- decode s.grids
    data_ <- decode s.data
    minzoom <- unNullOrUndefined <$> decode s.minzoom
    maxzoom <- unNullOrUndefined <$> decode s.maxzoom
    bounds <- unNullOrUndefined <$> decode s.bounds
    center <- unNullOrUndefined <$> decode s.center
    pure (TileJSON{
        name, description, version, attribution, template,
        legend, scheme, tiles, grids, data:data_, minzoom,
        maxzoom, bounds, center
    })

data TileScheme = XYZ | TMS

instance encodeTileScheme :: Encode TileScheme where
  encode XYZ = toForeign "xyz"
  encode TMS = toForeign "tms"

instance decodeTileScheme :: Decode TileScheme where
  decode s = decode s >>= \s' -> case s' of
    "xyz" -> pure XYZ
    "tms" -> pure TMS
    _ -> fail (ForeignError "Invalid scheme")

newtype MustacheTemplate = MustacheTemplate String

instance encodeMustacheTemplate :: Encode MustacheTemplate where
  encode (MustacheTemplate t) = toForeign t

instance decodeMustacheTemplate :: Decode MustacheTemplate where
  decode = map MustacheTemplate <<< decode