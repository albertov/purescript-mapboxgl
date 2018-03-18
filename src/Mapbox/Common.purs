module Mapbox.Common where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Int (fromNumber, toNumber)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

newtype URI = URI String

type Zoom = Number

mkURI :: String -> Maybe URI
mkURI = Just <<< URI --TODO

printURI :: URI -> String
printURI (URI u) = u

instance encodeURI :: Encode URI where
    encode = toForeign <<< printURI

instance decodeURI :: Decode URI where
    decode = map URI <<< decode


newtype LonLat = LonLat { lon :: Number, lat :: Number }

instance encodeLonLat :: Encode LonLat where
    encode (LonLat ll) = toForeign [ll.lon, ll.lat]

instance decodeLonLat :: Decode LonLat where
    decode o = decode o >>= \a ->
        case a of
            [lon,lat] -> pure (LonLat {lon,lat})
            _         -> fail (ForeignError "invalid LonLat")

newtype Bounds = Bounds { southwest :: LonLat, northeast :: LonLat }

instance encodeBounds :: Encode Bounds where
  encode (Bounds {southwest:LonLat sw, northeast:LonLat ne}) =
    toForeign [sw.lon, sw.lat, ne.lon, ne.lat]

instance decodeBounds :: Decode Bounds where
    decode o = decode o >>= \a ->
        case a of
            [w,s,e,n] -> pure (Bounds {southwest:LonLat {lon:w,lat:s}, northeast:LonLat {lon:e,lat:n}})
            _         -> fail (ForeignError "invalid Bounds")

newtype LonLatZoom = LonLatZoom { lon :: Number, lat :: Number, zoom :: Int }

instance encodeLonLatZoom :: Encode LonLatZoom where
    encode (LonLatZoom llz) = toForeign [llz.lon, llz.lat, toNumber llz.zoom]

instance decodeLonLatZoom :: Decode LonLatZoom where
    decode o = decode o >>= \a ->
        case a of
            [lon,lat,zoom']
                | Just zoom <- fromNumber zoom' -> pure (LonLatZoom {lon,lat,zoom})
            _         -> fail (ForeignError "invalid LonLatZoom")


newtype Color = Color String
derive newtype instance encodeColor :: Encode Color
derive newtype instance decodeColor :: Decode Color
derive newtype instance fromValueColor :: FromValue Color
derive newtype instance toValueColor :: ToValue Color

            

newtype Transition = Transition
  { duration :: Maybe Int
  , delay    :: Maybe Int
  }

instance encodeTransition :: Encode Transition where
  encode (Transition{delay,duration}) = toForeign
    { delay : encode (NullOrUndefined delay)
    , duration : encode (NullOrUndefined duration)
    }

instance decodeTransition :: Decode Transition where
  decode o = readStrMap o *> do
    let t = unsafeFromForeign o
    delay <- unNullOrUndefined <$> decode t.delay
    duration <- unNullOrUndefined <$> decode t.duration
    pure (Transition {delay,duration})

data Anchor = Viewport | Map

instance encodeAnchor :: Encode Anchor where
  encode Viewport = toForeign "viewport"
  encode Map = toForeign "map"

instance decodeAnchor :: Decode Anchor where
  decode s = decode s >>= \s' -> case s' of
    "map" -> pure Map
    "viewport" -> pure Viewport
    _ -> fail (ForeignError "Invalid anchor")

instance anchorToValue :: ToValue Anchor where
  toValue Viewport = toValue "viewport"
  toValue Map = toValue "map"

instance anchorFromValue :: FromValue Anchor where
  fromValue s = fromValue s >>= \s' -> case s' of
    "map" -> pure Map
    "viewport" -> pure Viewport
    _ -> Left "Invalid anchor"


newtype Light = Light
    { anchor    :: Maybe Anchor
    , position  :: Maybe Position
    , color     :: Maybe Color
    , intensity :: Maybe Ratio
    }

instance encodeLight :: Encode Light where
  encode (Light l) = toForeign
    { anchor     : encode (NullOrUndefined l.anchor)
    , position : encode (NullOrUndefined l.position)
    , color : encode (NullOrUndefined l.color)
    , intensity : encode (NullOrUndefined l.intensity)
    }
instance decodeLight :: Decode Light where
    decode o = readStrMap o *> do
      let m = unsafeFromForeign o
      anchor <- unNullOrUndefined <$> decode m.anchor
      position <- unNullOrUndefined <$> decode m.position
      color <- unNullOrUndefined <$> decode m.color
      intensity <- unNullOrUndefined <$> decode m.intensity
      pure (Light {anchor,position,color,intensity})



newtype Position = Position
    { distance  :: Number
    , azimuth   :: Number
    , elevation :: Number
    }

instance encodePosition :: Encode Position where
    encode (Position {distance,azimuth,elevation}) = toForeign [distance,azimuth,elevation]
instance decodePosition :: Decode Position where
    decode o = decode o >>= \a ->
        case a of
            [distance,azimuth,elevation] -> pure (Position {distance,azimuth,elevation})
            _                            -> fail (ForeignError "invalid Position")




-- | Ratio is a number between 0 and 1
newtype Ratio = Ratio Number

instance encodeRatio :: Encode Ratio where
  encode (Ratio n) = toForeign n

instance decodeRatio :: Decode Ratio where
  decode = maybe (fail (ForeignError "Invalid ratio")) pure
         <=< map toRatio <<< decode

derive newtype instance toValueRatio :: ToValue Ratio
derive newtype instance fromValueRatio :: FromValue Ratio

toRatio :: Number -> Maybe Ratio
toRatio n | 0.0 <= n && n <= 1.0 = Just (Ratio n)
toRatio _ = Nothing

getRatio :: Ratio -> Number
getRatio (Ratio n ) = n

error :: forall x. String -> F x
error k = throwError $ singleton
    $ ForeignError
    $ "Expected '" <> k <> "' key"

required :: forall a. Decode a => String -> Foreign -> F a
required name = maybe (error name) pure
            <=< (map unNullOrUndefined <<< decode)


data Value
  = String String
  | StringArray (Array String)
  | Number Number
  | NumberArray (Array Number)
  | Boolean Boolean

class ToValue a where
  toValue :: a -> Value

instance stringtoValue :: ToValue String where toValue = String
instance stringArraytoValue :: ToValue (Array String) where toValue = StringArray
instance numbertoValue :: ToValue Number where toValue = Number
instance numberArraytoValue :: ToValue (Array Number) where toValue = NumberArray
instance booleantoValue :: ToValue Boolean where toValue = Boolean

class FromValue a where
  fromValue :: Value -> Either String a

instance stringfromValue :: FromValue String where
  fromValue (String s) = Right s
  fromValue _          = Left "Not a string"

instance stringArrayfromValue :: FromValue (Array String) where
  fromValue (StringArray s) = Right s
  fromValue _               = Left "Not a string array"

instance numberfromValue :: FromValue Number where
  fromValue (Number s) = Right s
  fromValue _          = Left "Not a number"

instance numberArrayfromValue :: FromValue (Array Number) where
  fromValue (NumberArray s) = Right s
  fromValue _               = Left "Not a number array"

instance booleanfromValue :: FromValue Boolean where
  fromValue (Boolean s) = Right s
  fromValue _           = Left "Not a boolean"

instance encodeValue :: Encode Value where
  encode (String      v) = toForeign v
  encode (StringArray v) = toForeign v
  encode (Number      v) = toForeign v
  encode (NumberArray v) = toForeign v
  encode (Boolean     v) = toForeign v

instance decodeValue :: Decode Value where
  decode v = String      <$> decode v
         <|> StringArray <$> decode v
         <|> Number      <$> decode v
         <|> NumberArray <$> decode v
         <|> Boolean     <$> decode v
                


pairs :: forall a. Array a -> Maybe (Array (Tuple a a))
pairs = pairsImpl Just Nothing Tuple

foreign import pairsImpl
  :: forall a
   . (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> (forall r s. r -> s -> Tuple r s)
  -> Array a
  -> Maybe (Array (Tuple a a))