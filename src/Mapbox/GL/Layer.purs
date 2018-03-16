module Mapbox.GL.Layer where



import Prelude

import Data.Foreign (Foreign, ForeignError(ForeignError), F, fail, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined, undefined)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.StrMap (StrMap, empty, lookup)
import Mapbox.Common (Color, Ratio, Transition, Zoom, error)
import Mapbox.GL.Expression (Expression, Value)

data Layer
  = Fill
    { id          :: String
    , visibility  :: Maybe Visibility
    , metadata    :: Maybe (StrMap Value)
    , source      :: String
    , sourceLayer :: Maybe String
    , minzoom     :: Maybe Int
    , maxzoom     :: Maybe Int
    , filter      :: Maybe Expression

    , antialias   :: Maybe (Property Boolean)
    , opacity     :: Maybe (Transitionable (Property Ratio))
    , color       :: Maybe (Transitionable (Property Color))
    }


instance encodeLayer :: Encode Layer where
  encode (Fill l) = toForeign
    { id : toForeign l.id

    }

instance decodeLayer :: Decode Layer where
    decode o = readStrMap o *> do
      let m = unsafeFromForeign o
      id <- maybe (error "id") pure
             =<< unNullOrUndefined <$> decode m.id
      metadata <- unNullOrUndefined <$> decode m.metadata
      minzoom <- unNullOrUndefined <$> decode m.minzoom
      maxzoom <- unNullOrUndefined <$> decode m.maxzoom
      filter <- unNullOrUndefined <$> decode m.filter
      sourceLayer <- unNullOrUndefined <$> decode m.sourceLayer
      mSource <- unNullOrUndefined <$> decode m.source

      paint <- (fromMaybe empty <<< unNullOrUndefined) <$> decode m.paint
      layout <- (fromMaybe empty <<< unNullOrUndefined) <$> decode m.layout
      visibility <- maybe (pure Nothing) decode ("visibility" `lookup` layout)
      
      type_ <- maybe (error "type") pure
             =<< unNullOrUndefined <$> decode m.type
      case type_ of
        "fill" -> do
          source <- maybe (error "source") pure mSource
          antialias <- getProp paint "fill-antialias"
          opacity <- getTransitionableProp paint  "fill-opacity"
          color <- getTransitionableProp paint "fill-color"
          pure (Fill{id,visibility,metadata,source,sourceLayer,minzoom,maxzoom,filter,antialias,opacity,color})
        unknown -> fail (ForeignError ("Unknown layer type: " <> unknown))


getProp :: forall t7. Decode t7 => StrMap Foreign -> String -> F (Maybe t7)
getProp m prop =
  maybe (pure Nothing)
        (map unNullOrUndefined <<< decode)
        (prop `lookup`  m)

getTransitionableProp :: forall t33. Decode t33 => StrMap Foreign -> String -> F (Maybe (Transitionable t33))
getTransitionableProp m prop = do
  mo <- getProp m prop
  case mo of
    Just o -> Just <$> (Transitionable <$> pure o <*> getProp m (prop<>"-transition"))
    Nothing -> pure Nothing


data Transitionable o = Transitionable o (Maybe Transition)


data Visibility = None | Visible

instance encodeVisibility :: Encode Visibility where
  encode None = toForeign "none"
  encode Visible = toForeign "visible"

instance decodeVisibility :: Decode Visibility where
  decode s = decode s >>= \s' -> case s' of
    "none" -> pure None
    "visible" -> pure Visible
    _ -> fail (ForeignError ("Invalid visibility: " <> s'))

data FunctionType
  = IdentityFun
  | ExponentialFun
  | IntervalFun
  | CategoricalFun

instance encodeFunctionType :: Encode FunctionType where
  encode IdentityFun = toForeign "identity"
  encode ExponentialFun = toForeign "exponential"
  encode IntervalFun = toForeign "interval"
  encode CategoricalFun = toForeign "categorical"

data ColorSpace = RGBSpace | LabSpace | HclSpace

instance encodeColorSpace :: Encode ColorSpace where
  encode RGBSpace = toForeign "rgb"
  encode LabSpace = toForeign "lab"
  encode HclSpace = toForeign "hcl"

data Property o
  = Prop Expression
  | Function
    { stops    :: Stops
    , base     :: Maybe Number
    , type     :: Maybe FunctionType
    , default  :: Maybe Expression
    , colorSpace :: Maybe ColorSpace
    }

instance decodeProperty :: Decode (Property o) where
  decode v = decodeProp
    where
    decodeProp = Prop <$> decode v

instance encodeProperty :: Encode (Property o) where
  encode (Prop e) = encode e
  encode (Function f) = toForeign
    { stops      : encode f.stops
    , base       : encode (NullOrUndefined f.base)
    , type       : encode (NullOrUndefined f.type)
    , default    : encode (NullOrUndefined f.default)
    , colorSpace : encode (NullOrUndefined f.colorSpace)
    , property   : case f.stops of
        PropStops p _ -> encode p
        ZoomPropStops p _ -> encode p
        _                 -> undefined
    }

data ZoomStop = ZoomStop Zoom Expression

instance encodeZoomStop :: Encode ZoomStop where
  encode (ZoomStop z e) = toForeign [encode z, encode e]


data PropStop = PropStop Value Expression

instance encodePropStop :: Encode PropStop where
  encode (PropStop z e) = toForeign [encode z, encode e]


data ZoomPropStop = ZoomPropStop Zoom Value Expression

instance encodeZoomPropStop :: Encode ZoomPropStop where
  encode (ZoomPropStop z v e) = toForeign [toForeign {zoom:encode z, value:encode v}, encode e]


data Stops
  =  ZoomStops            (Array ZoomStop)
  |  PropStops     String (Array PropStop)
  |  ZoomPropStops String (Array ZoomPropStop)

instance encodeStops :: Encode Stops where
  encode (ZoomStops a) = encode a
  encode (PropStops _ a) = encode a
  encode (ZoomPropStops _ a) = encode a