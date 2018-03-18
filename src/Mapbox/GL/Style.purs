module Mapbox.GL.Style (
  Style (..)
) where

import Data.Foreign (Foreign, ForeignError(ForeignError), fail, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Mapbox.Common (Light, LonLat, Transition, URI, Zoom, required)
import Mapbox.GL.Layer (Layer)
import Mapbox.GL.Source (Source)
import Prelude (bind, discard, pure, unless, ($), (*>), (<$>), (=<<), (==))


newtype Style = Style
    { version    :: Int
    , name       :: Maybe String
    , metadata   :: Maybe (StrMap Foreign)
    , center     :: Maybe LonLat
    , zoom       :: Maybe Zoom
    , bearing    :: Maybe Number
    , pitch      :: Maybe Number
    , light      :: Maybe Light
    , sources    :: StrMap Source
    , sprite     :: Maybe URI
    , glyphs     :: Maybe URI
    , transition :: Maybe Transition
    , layers     :: Array Layer
    }

instance encodeStyle :: Encode Style where
  encode (Style s) = toForeign
    { version  : s.version
    , name     : encode (NullOrUndefined s.name)
    , metadata : encode (NullOrUndefined s.metadata)
    , center : encode (NullOrUndefined s.center)
    , zoom : encode (NullOrUndefined s.zoom)
    , bearing : encode (NullOrUndefined s.bearing)
    , pitch : encode (NullOrUndefined s.pitch)
    , light : encode (NullOrUndefined s.light)
    , sources : encode s.sources
    , sprite   : encode (NullOrUndefined s.sprite)
    , glyphs   : encode (NullOrUndefined s.glyphs)
    , transition: encode (NullOrUndefined s.transition)
    , layers : encode s.layers
    }


instance decodeStyle :: Decode Style where
    decode o = readStrMap o *> do
      let m = unsafeFromForeign o
      version <- required "version" m.version
      unless (version == 8) $
        fail (ForeignError "Expected version 8")
      name <- unNullOrUndefined <$> decode m.name
      metadata <- unNullOrUndefined <$> decode m.metadata
      center <- unNullOrUndefined <$> decode m.center
      zoom <- unNullOrUndefined <$> decode m.zoom
      bearing <- unNullOrUndefined <$> decode m.bearing
      pitch <- unNullOrUndefined <$> decode m.pitch
      light <- unNullOrUndefined <$> decode m.light
      sources <- required "sources" m.sources
      layers <- traverse decode
            =<< (derefLayers <$> required "layers" m.layers)
      sprite <- unNullOrUndefined <$> decode m.sprite
      glyphs <- unNullOrUndefined <$> decode m.glyphs
      transition <- unNullOrUndefined <$> decode m.transition
      pure (Style { version
                  , name
                  , metadata
                  , center
                  , zoom
                  , bearing
                  , pitch
                  , light
                  , sources
                  , sprite
                  , glyphs
                  , transition
                  , layers
                  })

foreign import derefLayers :: Array Foreign -> Array Foreign