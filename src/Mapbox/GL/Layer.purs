module Mapbox.GL.Layer (
  Layer(..)
, Property
, Transitionable
, Visibility (..)
, SpriteRef (..)
, SourceRef (..)
, LineCap (..)
, LineJoin (..)
, Justify (..)
, SymbolPlacement (..)
, BoxAnchor (..)
, Alignment (..)
, TextFit (..)
, TextTransform (..)
, DashArray (..)
, XY (..)
, Translate
, Padding
, Offset
, Pixels
, Factor
, Degrees
, Ems
, prop
) where



import Prelude

import Control.Alt ((<|>))
import Data.Array (concatMap)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(ForeignError), F, fail, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined, undefined)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.StrMap (StrMap, empty, lookup)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Mapbox.Common (class FromValue, class ToValue, Anchor, Color, Ratio, Transition, Value, Zoom, fromValue, pairs, required, toValue)
import Mapbox.GL.Expression (Expr, lit)

data SourceRef = SourceRef String | VectorSourceRef String String

newtype SpriteRef = SpriteRef String

derive newtype instance encodeSpriteRef :: Encode SpriteRef
derive newtype instance decodeSpriteRef :: Decode SpriteRef
derive newtype instance toValueSpriteRef :: ToValue SpriteRef
derive newtype instance fromValueSpriteRef :: FromValue SpriteRef

type Pixels = Number
type Ems = Number
type Factor = Number
type Degrees = Number

data Layer
  = Background
    { id          :: String
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe Expr

    , color       :: Maybe (Transitionable (Property Color))
    , pattern     :: Maybe (Transitionable (Property SpriteRef))
    , opacity     :: Maybe (Transitionable (Property Ratio))
    }
  | Fill
    { id          :: String
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe Expr
    , source      :: SourceRef

    , antialias   :: Maybe (Property Boolean)
    , opacity     :: Maybe (Transitionable (Property Ratio))
    , color       :: Maybe (Transitionable (Property Color))
    }
  | Line
    { id          :: String
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe Expr
    , source      :: SourceRef

    , cap         :: Maybe (Property LineCap)
    , join        :: Maybe (Property LineJoin)
    , miterLimit  :: Maybe (Property Pixels)
    , roundLimit  :: Maybe (Property Pixels)
    , opacity     :: Maybe (Transitionable (Property Ratio))
    , color       :: Maybe (Transitionable (Property Color))
    , translate   :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor :: Maybe (Property Anchor)
    , width  :: Maybe (Transitionable (Property Pixels))
    , gapWidth  :: Maybe (Transitionable (Property Pixels))
    , offset  :: Maybe (Transitionable (Property Pixels))
    , blur  :: Maybe (Transitionable (Property Pixels))
    , dashArray  :: Maybe (Transitionable (Property DashArray))
    , pattern     :: Maybe (Transitionable (Property SpriteRef))
    }
  | Symbol
    { id          :: String
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe Expr
    , source      :: SourceRef

    , placement    :: Maybe (Property SymbolPlacement)
    , spacing      :: Maybe (Property Pixels)
    , avoidEdges   :: Maybe (Property Boolean)
    , allowOverlap :: Maybe (Property Boolean)
    , ignorePlacement :: Maybe (Property Boolean)
    , optional        :: Maybe (Property Boolean)
    , rotationAlignment :: Maybe (Property Alignment)
    , size              :: Maybe (Property Factor)
    , textFit           :: Maybe (Property TextFit)
    , textFitPadding    :: Maybe (Property (XY Padding))
    , image             :: Maybe (Property SpriteRef)
    , rotate            :: Maybe (Property Degrees)
    , padding          :: Maybe (Property Pixels)
    , keepUpright     :: Maybe (Property Boolean)
    , offset          :: Maybe (Property (XY Offset))
    , anchor          :: Maybe (Property BoxAnchor)
    , pitchAlignment :: Maybe (Property Alignment)
    , textPitchAlignment :: Maybe (Property Alignment)
    , textRotationAlignment :: Maybe (Property Alignment)
    , textField             :: Maybe (Property String)
    , textFont             :: Maybe (Property (Array String))
    , textSize             :: Maybe (Property Pixels)
    , textMaxWidth             :: Maybe (Property Ems)
    , textLineHeight             :: Maybe (Property Ems)
    , textLetterSpacing             :: Maybe (Property Ems)
    , textJustify             :: Maybe (Property Justify)
    , textAnchor             :: Maybe (Property BoxAnchor)
    , textMaxAngle             :: Maybe (Property Degrees)
    , textRotate             :: Maybe (Property Degrees)
    , textPadding             :: Maybe (Property Pixels)
    , textKeepUpright         :: Maybe (Property Boolean)
    , textTransform         :: Maybe (Property TextTransform)
    , textOffset         :: Maybe (Property (XY Offset))
    , textAllowOverlap         :: Maybe (Property Boolean)
    , textIgnorePlacement         :: Maybe (Property Boolean)
    , textOptional         :: Maybe (Property Boolean)
    , opacity     :: Maybe (Transitionable (Property Ratio))
    , color       :: Maybe (Transitionable (Property Color))
    , haloColor       :: Maybe (Transitionable (Property Color))
    , haloWidth       :: Maybe (Transitionable (Property Pixels))
    , haloBlur       :: Maybe (Transitionable (Property Pixels))
    , translate       :: Maybe (Transitionable (Property (XY Translate)))
    , translateAnchor         :: Maybe (Property Anchor)
    , textOpacity :: Maybe (Transitionable (Property Ratio))
    , textColor       :: Maybe (Transitionable (Property Color))
    , textHaloColor       :: Maybe (Transitionable (Property Color))
    , textHaloWidth       :: Maybe (Transitionable (Property Pixels))
    , textHaloBlur       :: Maybe (Transitionable (Property Pixels))
    , textTranslate       :: Maybe (Transitionable (Property (XY Translate)))
    , textTranslateAnchor         :: Maybe (Property Anchor)
    }
  | Raster
    { id          :: String
    , visibility  :: Maybe (Property Visibility)
    , metadata    :: Maybe (StrMap Value)
    , minzoom     :: Maybe Zoom
    , maxzoom     :: Maybe Zoom
    , filter      :: Maybe Expr
    , source      :: SourceRef

    , opacity     :: Maybe (Transitionable (Property Ratio))
    , hueRotate     :: Maybe (Transitionable (Property Degrees))
    , brightnessMin     :: Maybe (Transitionable (Property Ratio))
    , brightnessMax     :: Maybe (Transitionable (Property Ratio))
    , saturation     :: Maybe (Transitionable (Property Number)) --TODO use a newtype for [-1,-1]
    , contrast     :: Maybe (Transitionable (Property Number)) --TODO ditto
    , fadeDuration     :: Maybe (Property Milliseconds) --TODO ditto
    }


instance encodeLayer :: Encode Layer where
  encode (Background l) = toForeign
    { id : toForeign l.id
    }
  encode (Fill l) = toForeign
    { id : toForeign l.id
    }
  encode (Line l) = toForeign
    { id : toForeign l.id
    }
  encode (Symbol l) = toForeign
    { id : toForeign l.id
    }
  encode (Raster l) = toForeign
    { id : toForeign l.id
    }

instance decodeLayer :: Decode Layer where
    decode o = readStrMap o *> do
      let m = unsafeFromForeign o
      id <- required "id" m.id
      metadata <- unNullOrUndefined <$> decode m.metadata
      minzoom <- unNullOrUndefined <$> decode m.minzoom
      maxzoom <- unNullOrUndefined <$> decode m.maxzoom
      filter <- unNullOrUndefined <$> decode m.filter

      paint <- (fromMaybe empty <<< unNullOrUndefined) <$> decode m.paint
      layout <- (fromMaybe empty <<< unNullOrUndefined) <$> decode m.layout
      visibility <- maybe (pure Nothing) decode ("visibility" `lookup` layout)
      
      type_ <- required "type" m.type
      case type_ of
        "background" -> do
          color <- getTransitionableProp paint "background-color"
          pattern <- getTransitionableProp paint "background-pattern"
          opacity <- getTransitionableProp paint  "background-opacity"
          pure (Background{id,visibility,metadata,minzoom,maxzoom,filter,pattern,opacity,color})
        "fill" -> do
          source <- decodeSourceRef m
          antialias <- getProp paint "fill-antialias"
          opacity <- getTransitionableProp paint  "fill-opacity"
          color <- getTransitionableProp paint "fill-color"
          pure (Fill{id,visibility,metadata,source,minzoom,maxzoom,filter,antialias,opacity,color})
        "line" -> do
          source <- decodeSourceRef m
          cap <- getProp layout "line-cap"
          join <- getProp layout "line-join"
          miterLimit <- getProp layout "line-miter-limit"
          roundLimit <- getProp layout "line-round-limit"
          opacity <- getTransitionableProp paint  "line-opacity"
          color <- getTransitionableProp paint "line-color"
          translate <- getTransitionableProp paint "line-translate"
          translateAnchor <- getProp paint "line-translate-anchor"
          width <- getTransitionableProp paint  "line-width"
          gapWidth <- getTransitionableProp paint  "line-gap-width"
          offset <- getTransitionableProp paint  "line-offset"
          blur <- getTransitionableProp paint  "line-blur"
          dashArray <- getTransitionableProp paint  "line-dasharray"
          pattern <- getTransitionableProp paint  "line-pattern"
          pure (Line{ id,visibility,metadata,source,minzoom,maxzoom,filter,cap,join
                    , miterLimit,roundLimit,opacity,color
                    , translate, translateAnchor,width,gapWidth,offset,blur,dashArray,pattern})
        "symbol" -> do
          source <- decodeSourceRef m
          placement <- getProp layout "symbol-placement"
          spacing   <- getProp layout "symbol-spacing"
          avoidEdges <- getProp layout "symbol-avoid-edges"
          allowOverlap <- getProp layout "icon-allow-overlap"
          ignorePlacement <- getProp layout "icon-ignore-placement"
          optional <- getProp layout "icon-optional"
          rotationAlignment <- getProp layout "icon-rotation-alignment"
          size <- getProp layout "icon-size"
          textFit <- getProp layout "icon-text-fit"
          textFitPadding <- getProp layout "icon.text-fit-padding"
          image <- getProp layout "icon-image"
          rotate <- getProp layout "icon-rotate"
          padding <- getProp layout "icon-padding"
          keepUpright <- getProp layout "icon-keep-upright"
          offset <- getProp layout "icon-offset"
          anchor <- getProp layout "icon-anchor"
          pitchAlignment <- getProp layout "icon-pitch-alignment"
          textPitchAlignment <- getProp layout "text-pitch-alignment"
          textRotationAlignment <- getProp layout "text-rotation-alignment"
          textField <- getProp layout "text-field"
          textFont <- getProp layout "text-font"
          textSize <- getProp layout "text-size"
          textMaxWidth <- getProp layout "text-max-width"
          textLineHeight <- getProp layout "text-max-height"
          textLetterSpacing <- getProp layout "text-letter-spacing"
          textJustify <- getProp layout "text-justify"
          textAnchor <- getProp layout "text-anchor"
          textMaxAngle <- getProp layout "text-max-angle"
          textRotate <- getProp layout "text-rotate"
          textPadding <- getProp layout "text-padding"
          textKeepUpright <- getProp layout "text-keep-upright"
          textTransform <- getProp layout "text-transform"
          textOffset <- getProp layout "text-offset"
          textAllowOverlap <- getProp layout "text-allow-overlap"
          textIgnorePlacement <- getProp layout "text-ignore-placement"
          textOptional <- getProp layout "text-optional"
          opacity <- getTransitionableProp paint "icon-opacity"
          color <- getTransitionableProp paint "icon-color"
          haloColor <- getTransitionableProp paint "icon-halo-color"
          haloWidth <- getTransitionableProp paint "icon-halo-width"
          haloBlur <- getTransitionableProp paint "icon-halo-blur"
          translate <- getTransitionableProp paint "icon-translate"
          translateAnchor <- getProp paint "icon-translate-anchor"
          textOpacity <- getTransitionableProp paint "text-opacity"
          textColor <- getTransitionableProp paint "text-color"
          textHaloColor <- getTransitionableProp paint "text-halo-color"
          textHaloWidth <- getTransitionableProp paint "text-halo-width"
          textHaloBlur <- getTransitionableProp paint "text-halo-blur"
          textTranslate <- getTransitionableProp paint "text-translate"
          textTranslateAnchor <- getProp paint "text-translate-anchor"
          pure (Symbol { id , visibility , metadata , minzoom , maxzoom , filter , source , placement , spacing
                       , avoidEdges , allowOverlap , ignorePlacement , optional , rotationAlignment , size , textFit
                       , textFitPadding , image , rotate , padding , keepUpright , offset , anchor , pitchAlignment
                       , textPitchAlignment , textRotationAlignment , textField , textFont , textSize , textMaxWidth
                       , textLineHeight , textLetterSpacing , textJustify , textAnchor , textMaxAngle , textRotate
                       , textPadding , textKeepUpright , textTransform , textOffset , textAllowOverlap , textIgnorePlacement
                       , textOptional , opacity , color , haloColor , haloWidth , haloBlur , translate , translateAnchor
                       , textOpacity , textColor , textHaloColor , textHaloWidth , textHaloBlur , textTranslate
                       , textTranslateAnchor })
        "raster" -> do
          source <- decodeSourceRef m
          opacity <- getTransitionableProp paint "raster-opacity"
          hueRotate <- getTransitionableProp paint "raster-hue-rotate"
          brightnessMin <- getTransitionableProp paint "raster-brightness-min"
          brightnessMax <- getTransitionableProp paint "raster-brightness-max"
          saturation <- getTransitionableProp paint "raster-saturation"
          contrast <- getTransitionableProp paint "raster-contrast"
          fadeDuration <- getProp paint "raster-fade-duration"
          pure (Raster { id, visibility, metadata, minzoom, maxzoom, filter, source
                       , opacity, hueRotate, brightnessMin, brightnessMax, saturation
                       , contrast, fadeDuration})
        unknown -> fail (ForeignError ("Unknown layer type: " <> unknown))

decodeSourceRef
  :: forall o. {source :: Foreign , sourceLayer :: Foreign | o}
  -> F SourceRef
decodeSourceRef m = do
  source <- required "source" m.source
  sourceLayer <- unNullOrUndefined <$> decode m.sourceLayer
  case sourceLayer of
    Just sl -> pure (VectorSourceRef source sl)
    Nothing -> pure (SourceRef source)

getProp :: forall t7. Decode t7 => StrMap Foreign -> String -> F (Maybe t7)
getProp m p =
  maybe (pure Nothing)
        (map unNullOrUndefined <<< decode)
        (p `lookup`  m)

getTransitionableProp :: forall t33. Decode t33 => StrMap Foreign -> String -> F (Maybe (Transitionable t33))
getTransitionableProp m p = do
  mo <- getProp m p
  case mo of
    Just o -> Just <$> (Transitionable <$> pure o <*> getProp m (p<>"-transition"))
    Nothing -> pure Nothing


data Transitionable o = Transitionable o (Maybe Transition)


newtype DashArray = DashArray (Array (Tuple Pixels Pixels))
instance toValueDashArray :: ToValue DashArray where
  toValue (DashArray as) = toValue (concatMap (\(Tuple a b) -> [a, b]) as)
instance fromValueDashArray :: FromValue DashArray where
  fromValue a = fromValue a >>= \a' ->
    maybe (Left "expected even number of items in dash array") (pure<<<DashArray) (pairs a')

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

instance decodeFunctionType :: Decode FunctionType where
  decode s = decode s >>= \s' -> case s' of
    "identity" -> pure IdentityFun
    "exponential" -> pure ExponentialFun
    "interval" -> pure IntervalFun
    "categorical" -> pure CategoricalFun
    o -> fail (ForeignError ("Invalid function type: " <> o))

data ColorSpace = RGBSpace | LabSpace | HclSpace

instance encodeColorSpace :: Encode ColorSpace where
  encode RGBSpace = toForeign "rgb"
  encode LabSpace = toForeign "lab"
  encode HclSpace = toForeign "hcl"

instance decodeColorSpace :: Decode ColorSpace where
  decode s = decode s >>= \s' -> case s' of
    "rgb" -> pure RGBSpace
    "lab" -> pure LabSpace
    "hcl" -> pure HclSpace
    o -> fail (ForeignError ("Invalid color space: " <> o))

data Property o
  = Prop Expr
  | Function
    { stops    :: Stops
    , base     :: Maybe Number
    , type     :: Maybe FunctionType
    , default  :: Maybe Expr
    , colorSpace :: Maybe ColorSpace
    }

prop :: forall a. ToValue a => a -> Property a
prop = Prop <<< lit

instance decodeProperty :: Decode (Property o) where
  decode v = decodeProp
        <|> (readStrMap v *> decodeFun (unsafeFromForeign v))
    where
    decodeProp = Prop <$> decode v
    decodeFun o = do
      property <- unNullOrUndefined <$> decode o.property
      stops <- case property of
        Just p -> ZoomPropStops p <$> required "stops" o.stops
              <|> PropStops     p <$> required "stops" o.stops
        Nothing -> ZoomStops <$> required "stops" o.stops
      base <- unNullOrUndefined <$> decode o.base
      type_ <- unNullOrUndefined <$> decode o.type
      default <- unNullOrUndefined <$> decode o.default
      colorSpace <- unNullOrUndefined <$> decode o.colorSpace
      pure (Function {stops,base,type:type_,default,colorSpace})

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

data ZoomStop = ZoomStop Zoom Expr

instance encodeZoomStop :: Encode ZoomStop where
  encode (ZoomStop z e) = toForeign [encode z, encode e]

instance decodeZoomStop :: Decode ZoomStop where
  decode v = decode v >>= \ze ->
    case ze of
      [z, e] -> ZoomStop <$> decode z <*> decode e
      _      -> fail (ForeignError "Expected 2-item array for zoom function stop")


data PropStop = PropStop Value Expr

instance encodePropStop :: Encode PropStop where
  encode (PropStop z e) = toForeign [encode z, encode e]

instance decodePropStop :: Decode PropStop where
  decode v = decode v >>= \ze ->
    case ze of
      [z, e] -> PropStop <$> decode z <*> decode e
      _      -> fail (ForeignError "Expected 2-item array for property function stop")

data ZoomPropStop = ZoomPropStop Zoom Value Expr

instance encodeZoomPropStop :: Encode ZoomPropStop where
  encode (ZoomPropStop z v e) = toForeign [toForeign {zoom:encode z, value:encode v}, encode e]

instance decodeZoomPropStop :: Decode ZoomPropStop where
  decode v = decode v >>= \ze ->
    case ze of
      [z, e] -> readStrMap z *>  do
        let o = unsafeFromForeign z
        zoom <- required "zoom" o.zoom
        value <- required "value" o.value
        ZoomPropStop zoom value <$> decode e
        

      _      -> fail (ForeignError "Expected 2-item array for property function stop")

data Stops
  =  ZoomStops            (Array ZoomStop)
  |  PropStops     String (Array PropStop)
  |  ZoomPropStops String (Array ZoomPropStop)

instance encodeStops :: Encode Stops where
  encode (ZoomStops a) = encode a
  encode (PropStops _ a) = encode a
  encode (ZoomPropStops _ a) = encode a


data Visibility = None | Visible

instance visibilityToValue :: ToValue Visibility where
  toValue None = toValue "none"
  toValue Visible = toValue "visible"

instance visibilityFromValue :: FromValue Visibility where
  fromValue s = fromValue s >>= \s' -> case s' of
    "none" -> pure None
    "visible" -> pure Visible
    _ -> Left ("Invalid visibility: " <> s')

data LineCap = ButtCap | RoundCap | SquareCap

instance lineCapToValue :: ToValue LineCap where
  toValue ButtCap = toValue "butt"
  toValue RoundCap = toValue "round"
  toValue SquareCap = toValue "square"

instance lineCapFromCalue :: FromValue LineCap where
  fromValue s = fromValue s >>= \s' -> case s' of
    "butt" -> pure ButtCap
    "round" -> pure RoundCap
    "square" -> pure SquareCap
    _ -> Left ("Invalid line-cap: " <> s')

data LineJoin = BevelJoin | RoundJoin | MiterJoin

instance lineJoinToValue :: ToValue LineJoin where
  toValue BevelJoin = toValue "bevel"
  toValue RoundJoin = toValue "round"
  toValue MiterJoin = toValue "miter"

instance lineJoinFromValue :: FromValue LineJoin where
  fromValue s = fromValue s >>= \s' -> case s' of
    "bevel" -> pure BevelJoin
    "round" -> pure RoundJoin
    "miter" -> pure MiterJoin
    _ -> Left ("Invalid line-join: " <> s')

data Padding
data Translate
data Offset
data XY t = XY Pixels Pixels

instance xyToValue :: ToValue (XY t) where
  toValue (XY x y) = toValue [x,y]

instance xyFromValue :: FromValue (XY t) where
  fromValue s = fromValue s >>= \s' -> case s' of
    [x,y] -> pure (XY x y)
    _     -> Left "expected a 2-element array"


data SymbolPlacement = PointPlacement | LinePlacement

instance symbolPlacementToValue :: ToValue SymbolPlacement where
  toValue PointPlacement = toValue "point"
  toValue LinePlacement = toValue "line"

instance symbolPlacementFromValue :: FromValue SymbolPlacement where
  fromValue s = fromValue s >>= \s' -> case s' of
    "line" -> pure LinePlacement
    "point" -> pure PointPlacement
    _ -> Left ("Invalid symbol-placement: " <> s')

data Alignment = MapAlignment | ViewportAlignment | AutoAlignment

instance alignmentToValue :: ToValue Alignment where
  toValue MapAlignment = toValue "map"
  toValue ViewportAlignment = toValue "viewport"
  toValue AutoAlignment = toValue "auto"

instance alignmentFromValue :: FromValue Alignment where
  fromValue s = fromValue s >>= \s' -> case s' of
    "map" -> pure MapAlignment
    "viewport" -> pure ViewportAlignment
    "auto" -> pure AutoAlignment
    _ -> Left ("Invalid alignment: " <> s')

data TextFit = FitNone | FitWidth | FitHeight | FitBoth

instance textFitToValue :: ToValue TextFit where
  toValue FitNone = toValue "none"
  toValue FitWidth = toValue "width"
  toValue FitHeight = toValue "height"
  toValue FitBoth = toValue "both"

instance textFitFromValue :: FromValue TextFit where
  fromValue s = fromValue s >>= \s' -> case s' of
    "none" -> pure FitNone
    "width" -> pure FitWidth
    "height" -> pure FitHeight
    "both" -> pure FitBoth
    _ -> Left ("Invalid text-fit: " <> s')

data Justify = JustifyLeft | JustifyCenter | JustifyRight

instance justifyToValue :: ToValue Justify where
  toValue JustifyLeft = toValue "left"
  toValue JustifyCenter = toValue "center"
  toValue JustifyRight = toValue "right"

instance justifyFromValue :: FromValue Justify where
  fromValue s = fromValue s >>= \s' -> case s' of
    "left" -> pure JustifyLeft
    "center" -> pure JustifyCenter
    "right" -> pure JustifyRight
    _ -> Left ("Invalid justify: " <> s')

data BoxAnchor
  = AnchorCenter
  | AnchorLeft
  | AnchorRight
  | AnchorTop
  | AnchorBottom
  | AnchorTopLeft
  | AnchorTopRight
  | AnchorBottomLeft
  | AnchorBottomRight

instance boxAnchorToValue :: ToValue BoxAnchor where
  toValue AnchorCenter = toValue "center"
  toValue AnchorLeft = toValue "left"
  toValue AnchorRight = toValue "right"
  toValue AnchorTop = toValue "top"
  toValue AnchorBottom = toValue "bottom"
  toValue AnchorTopLeft = toValue "top-left"
  toValue AnchorTopRight = toValue "top-right"
  toValue AnchorBottomLeft = toValue "bottom-left"
  toValue AnchorBottomRight = toValue "bottom-right"

instance boxAnchorFromValue :: FromValue BoxAnchor where
  fromValue s = fromValue s >>= \s' -> case s' of
    "center" -> pure AnchorCenter
    "left" -> pure AnchorLeft
    "right" -> pure AnchorRight
    "top" -> pure AnchorTop
    "bottom" -> pure AnchorBottom
    "top-left" -> pure AnchorTopLeft
    "top-right" -> pure AnchorTopRight
    "bottom-left" -> pure AnchorBottomLeft
    "bottom-right" -> pure AnchorBottomRight
    _ -> Left ("Invalid anchor: " <> s')

data TextTransform
  = NoneTransform
  | LowercaseTransform
  | UppercaseTransform

instance textTransformToValue :: ToValue TextTransform where
  toValue NoneTransform = toValue "none"
  toValue LowercaseTransform = toValue "lowercase"
  toValue UppercaseTransform = toValue "uppercase"

instance textTransformFromValue :: FromValue TextTransform where
  fromValue s = fromValue s >>= \s' -> case s' of
    "lowercase" -> pure LowercaseTransform
    "uppercase" -> pure UppercaseTransform
    "none" -> pure NoneTransform
    _ -> Left ("Invalid text-transform: " <> s')