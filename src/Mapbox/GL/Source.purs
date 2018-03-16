module Mapbox.GL.Source where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable, uncons)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(..), F, fail, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.StrMap (insert)
import Mapbox.Common (LonLat, URI, Zoom, error, mkURI)
import Mapbox.TileJSON (TileJSON)

data Source
  = Vector    VectorSource
  | Raster    RasterSource
  | RasterDEM RasterSource
  | GeoJSON   GeoJSONSource
  | Image     ImageSource
  | Video     VideoSource
  | Canvas    CanvasSource

instance decodeSource :: Decode Source where
  decode o = readStrMap o *> do
    let s = unsafeFromForeign o
    type_ <- maybe (error "type") pure
         =<< unNullOrUndefined <$> decode s.type
    case type_ of
      "vector" ->
            urlWith (Vector<<<VectorUrl) s
        <|> Vector <<< VectorSource <$> decode o
      "raster" ->
            urlWith (Raster<<<RasterUrl) s
        <|> Raster <$> (RasterSource <$> decode o <*> (unNullOrUndefined <$> decode s.tileSize))
      "raster-dem" ->
            urlWith (RasterDEM<<<RasterUrl) s
        <|> RasterDEM <$> (RasterSource <$> decode o <*> (unNullOrUndefined <$> decode s.tileSize))
      "geojson" -> GeoJSON <$> decodeGeoJSONSource o
      "image" -> Image <$> decodeImageSource o
      "video" -> Video <$> decodeVideoSource o
      "canvas" -> Canvas <$> decodeCanvasSource o
      _ -> fail (ForeignError ("Unknown source type: " <> type_))


instance encodeSource :: Encode Source where
  encode (Vector src)    = encodeVectorSource  "vector"     src
  encode (Raster src)    = encodeRasterSource  "raster"     src
  encode (RasterDEM src) = encodeRasterSource  "raster-dem" src
  encode (GeoJSON src)   = encodeGeoJSONSource "geojson"    src
  encode (Image src)     = encodeImageSource   "image"      src
  encode (Video src)     = encodeVideoSource   "video"      src
  encode (Canvas src)    = encodeCanvasSource  "canvas"     src



urlWith :: forall t11 t4.
  (URI -> t4)
  -> { url :: Foreign
     | t11
     }
     -> F t4
urlWith f s = do
  mUrl <- unNullOrUndefined <$> decode s.url
  case mUrl of
    Just url'
      | Just url <- mkURI url' -> pure (f url)
      | otherwise -> fail (ForeignError "invalid url")
    Nothing -> fail (ForeignError "missing url")
            

data VectorSource
  = VectorUrl URI
  | VectorSource TileJSON


encodeVectorSource :: String -> VectorSource -> Foreign
encodeVectorSource type_ (VectorUrl uri) = toForeign
  { type : type_
  , url  : encode uri
  }
encodeVectorSource type_ (VectorSource tj) =
  let m = unsafeFromForeign (encode tj)
  in toForeign (insert "type" type_ m)

newtype TileSize = TileSize Int
instance encodeTileSize :: Encode TileSize where
  encode (TileSize v) = toForeign v
instance decodeTileSize :: Decode TileSize where
  decode o = decode o >>= \i ->
    if i > 0
    then pure (TileSize i)
    else fail (ForeignError "Invalid tileSize")

data RasterSource
  = RasterUrl URI
  | RasterSource TileJSON (Maybe TileSize)



encodeRasterSource :: String -> RasterSource -> Foreign
encodeRasterSource type_ (RasterUrl uri) = toForeign
    { type : toForeign type_
    , url  : encode uri
    }
encodeRasterSource type_ (RasterSource tj Nothing) =
  let m = unsafeFromForeign (encode tj)
  in toForeign (insert "type" type_ m)
encodeRasterSource type_ (RasterSource tj (Just ts)) =
  let m = unsafeFromForeign (encode tj)
  in toForeign
      $ insert "type" (toForeign type_)
      $ insert "tileSize" (encode ts) m

type GeoJSONData = Foreign

type GeoJSONSource =
  { data :: Either URI GeoJSONData
  , maxzoom :: Maybe Zoom
  , buffer :: Maybe Int
  , tolerance :: Maybe Number
  , cluster :: Maybe Boolean
  , clusterRadius :: Maybe Number
  , clusterMaxZoom :: Maybe Zoom
  }

encodeGeoJSONSource :: String -> GeoJSONSource -> Foreign
encodeGeoJSONSource type_ o = toForeign
  { type : type_
  , data : case o.data of
      Left uri -> encode uri
      Right gj -> gj
  , maxzoom: encode (NullOrUndefined o.maxzoom)
  , buffer : encode (NullOrUndefined o.buffer)
  , tolerance : encode (NullOrUndefined o.tolerance)
  , cluster : encode (NullOrUndefined o.cluster)
  , clusterRadius : encode (NullOrUndefined o.clusterRadius)
  , clusterMaxZoom : encode (NullOrUndefined o.clusterMaxZoom)
  }

decodeGeoJSONSource :: Foreign -> F GeoJSONSource
decodeGeoJSONSource o = readStrMap o *> do
  let s = unsafeFromForeign o
  data_ <- (Left <$> decode s.data)
        <|> pure (Right s.data)
  maxzoom <- unNullOrUndefined <$> decode s.maxzoom
  buffer <- unNullOrUndefined <$> decode s.buffer
  tolerance <- unNullOrUndefined <$> decode s.tolerance
  cluster <- unNullOrUndefined <$> decode s.cluster
  clusterRadius <- unNullOrUndefined <$> decode s.clusterRadius
  clusterMaxZoom <- unNullOrUndefined <$> decode s.clusterMaxZoom
  pure {data:data_, maxzoom, buffer, tolerance,cluster,clusterRadius,clusterMaxZoom}


type ImageCoordinates =
  { topLeft :: LonLat
  , topRight :: LonLat
  , bottomRight :: LonLat
  , bottomLeft :: LonLat
  }

encodeImageCoordinates :: ImageCoordinates -> Foreign
encodeImageCoordinates {topLeft:tl, topRight:tr, bottomRight:br, bottomLeft:bl} =
  toForeign [tl,tr,br,bl]

decodeImageCoordinates :: Foreign -> F ImageCoordinates
decodeImageCoordinates o = decode o >>= \a ->
  case a of
    [topLeft,topRight,bottomRight,bottomLeft] -> pure {topLeft,topRight,bottomRight,bottomLeft}
    _ -> fail (ForeignError "invalid image coordinates")


type ImageSource =
  { url :: URI
  , coordinates :: ImageCoordinates
  }

encodeImageSource :: String -> ImageSource -> Foreign
encodeImageSource type_ o = toForeign
  { type : type_
  , url : encode o.url
  , coordinates : encodeImageCoordinates o.coordinates
  }

decodeImageSource :: Foreign -> F ImageSource
decodeImageSource o = readStrMap o *> do
  let s = unsafeFromForeign o
  url <- decode s.url
  coordinates <- decodeImageCoordinates s.coordinates
  pure {url,coordinates}

type VideoSource =
  { urls :: NonEmpty Array URI
  , coordinates :: ImageCoordinates
  }

encodeVideoSource :: String -> VideoSource -> Foreign
encodeVideoSource type_ o = toForeign
  { type : type_
  , urls : encode (fromFoldable o.urls)
  , coordinates : encodeImageCoordinates o.coordinates
  }

decodeVideoSource :: Foreign -> F VideoSource
decodeVideoSource o = readStrMap o *> do
  let s = unsafeFromForeign o
  urlsArr <- decode s.urls
  urls <- case uncons urlsArr of
    Just {head,tail} -> pure (NonEmpty head tail)
    Nothing -> fail (ForeignError "must provide at least one url for video source")
  coordinates <- decodeImageCoordinates s.coordinates
  pure {urls,coordinates}

type ElementId = String

type CanvasSource =
  { canvas      :: ElementId
  , coordinates :: ImageCoordinates
  , animate     :: Maybe Boolean
  }

encodeCanvasSource :: String -> CanvasSource -> Foreign
encodeCanvasSource type_ o = toForeign
  { type : type_
  , canvas : encode o.canvas
  , coordinates : encodeImageCoordinates o.coordinates
  , animate     : encode (NullOrUndefined o.animate)
  }

decodeCanvasSource :: Foreign -> F CanvasSource
decodeCanvasSource o = readStrMap o *> do
  let s = unsafeFromForeign o
  coordinates <- decodeImageCoordinates s.coordinates
  canvas      <- decode s.canvas
  animate     <- unNullOrUndefined <$> decode s.animate
  pure {canvas,animate,coordinates}