module Mapbox.GL.Expression where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concatMap, fromFoldable, uncons, unsnoc)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(ForeignError), F, fail, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Mapbox.Common (Color)

data ExpType = StringT | NumberT | BooleanT

instance encodeExpType :: Encode ExpType where
  encode StringT  = toForeign "string"
  encode NumberT  = toForeign "number"
  encode BooleanT = toForeign "boolean"

instance decodeExpType :: Decode ExpType where
  decode s = decode s >>= \s' -> case s' of
    "string" -> pure StringT
    "number" -> pure NumberT
    "boolean" -> pure BooleanT
    _ -> fail (ForeignError "Invalid expression type")


data Value
  = String String
  | Number Number
  | Boolean Boolean
  | ColorValue Color

instance encodeValue :: Encode Value where
  encode (String     v) = toForeign v
  encode (Number     v) = toForeign v
  encode (Boolean    v) = toForeign v
  encode (ColorValue v) = encode v

instance decodeValue :: Decode Value where
  decode v = ColorValue <$> decode v
         <|> String     <$> decode v
         <|> Number     <$> decode v
         <|> Boolean    <$> decode v

data ArrayCheck = ArrayCheck ExpType (Maybe Int)

data Expression
  = Value      Value -- This node does not exist in the official AST but we need
                     -- to inject constants into expressions
  | ArrayExp   Expression (Maybe ArrayCheck)
  | BooleanExp Expression (Array Expression)
  | LiteralExp (Either (StrMap Value) (Array Value))
  | NumberExp   Expression (Array Expression)
  | ObjectExp   Expression (Array Expression)
  | StringExp   Expression (Array Expression)
  | ToBoolean   Expression
  | ToColor     Expression (Array Expression)
  | ToNumber    Expression (Array Expression)
  | ToString    Expression
  | TypeOf      Expression
  | GeometryType
  | Id
  | Properties
  | At            Int Expression
  | Get           String (Maybe Expression)
  | Has           String (Maybe Expression)
  | Length        Expression
  | Not           Expression
  | NotEq         Expression Expression
  | LessThan      Expression Expression
  | LessThanEq    Expression Expression
  | Equal         Expression Expression
  | GreaterThan   Expression Expression
  | GreaterThanEq Expression Expression
  | All           Expression Expression (Array Expression)
  | Any           Expression Expression (Array Expression)
  | Case          (Array (Tuple Expression Expression)) Expression
  | Coalesce      Expression Expression (Array Expression)
  | Match         Expression (Array (Tuple (NonEmpty Array Expression) Expression)) Expression
  | Interpolate   InterpolationType Expression (Array (Tuple Number Expression))
  | Step          Expression Expression (Array (Tuple Number Expression))
  | Let           (Array (Tuple String Expression))
  | Var           String
  | Concat        Expression Expression (Array Expression)
  | Downcase      Expression
  | Upcase        Expression
  | Rgb           Expression Expression Expression
  | Rgba          Expression Expression Expression Expression
  | ToRgba        Expression
  | Minus         Expression Expression
  | Mult          Expression Expression
  | Div           Expression Expression
  | Mod           Expression Expression
  | Exp           Expression Expression
  | Plus          Expression Expression
  | Acos          Expression
  | Asin          Expression
  | Atan          Expression
  | Cos           Expression
  | E
  | Ln            Expression
  | Ln2           Expression
  | Log10         Expression
  | Log2          Expression
  | Max           Expression Expression (Array Expression)
  | Min           Expression Expression (Array Expression)
  | Pi
  | Sin           Expression
  | Sqrt          Expression
  | Tan           Expression
  | Zoom
  | HeatmapDensity


instance encodeExpression :: Encode Expression where
  encode (Value v) = encode v
  encode (ArrayExp e Nothing) = unOp "array" e
  encode (ArrayExp e (Just (ArrayCheck t Nothing))) = toForeign
    [ toForeign "array", encode e, encode t]
  encode (ArrayExp e (Just (ArrayCheck t (Just l)))) = toForeign
    [ toForeign "array", encode e, encode t, encode l]
  encode (BooleanExp e es) = unOpArgs "boolean" e es
  encode (LiteralExp (Left v)) = toForeign [toForeign "literal", encode v]
  encode (LiteralExp (Right v)) = toForeign [toForeign "literal", encode v]
  encode (NumberExp e es) = unOpArgs "number" e es
  encode (ObjectExp e es) = unOpArgs "object" e es
  encode (StringExp e es) = unOpArgs "string" e es
  encode (ToBoolean e) = unOp "to-boolean" e
  encode (ToColor e es) = unOpArgs "to-color" e es
  encode (ToNumber e es) = unOpArgs "to-number" e es
  encode (ToString e) = unOp "to-string" e
  encode (TypeOf e) = unOp "typeof" e
  encode GeometryType = constant "geometry-type"
  encode Id = constant "id"
  encode Properties = constant "properties"
  encode (At i e) = toForeign [toForeign "at", toForeign i, encode e]
  encode (Get s Nothing) = toForeign ["get", s]
  encode (Get s (Just e)) = toForeign [toForeign "get", toForeign s, encode e]
  encode (Has s Nothing) = toForeign ["has", s]
  encode (Has s (Just e)) = toForeign [toForeign "has", toForeign s, encode e]
  encode (Length e) = unOp "length" e
  encode (Not e) = unOp "!" e
  encode (NotEq a b) = binOp "!=" a b
  encode (LessThan a b) = binOp "<" a b
  encode (LessThanEq a b) = binOp "<=" a b
  encode (Equal a b) = binOp "==" a b
  encode (GreaterThan a b) = binOp ">" a b
  encode (GreaterThanEq a b) = binOp ">=" a b
  encode (All a b xs) = binOpArgs "all" a b xs
  encode (Any a b xs) = binOpArgs "any" a b xs
  encode (Case cs d) = toForeign  ([toForeign "case"] <> concatMap (\(Tuple c v) -> [encode c, encode v]) cs <> [encode d])
  encode (Coalesce a b xs) = toForeign  ([toForeign "coalesce", encode a, encode b] <> map encode xs)
  encode (Match i cs d) = toForeign  ([toForeign "match", encode i] <> concatMap encodeMatchCase cs <> [encode d])
    where encodeMatchCase (Tuple (NonEmpty x []) v) = [encode x, encode v]
          encodeMatchCase (Tuple labels v) = [encode (fromFoldable labels) , encode v]
  encode (Interpolate t e stops) =
    toForeign (   [toForeign "interpolate", encode t, encode e]
               <> concatMap (\(Tuple a b) -> [encode a, encode b]) stops)
  encode (Step i d steps) = toForeign  ([toForeign "step", encode i] <> concatMap (\(Tuple i o) -> [toForeign i, encode o]) steps <> [encode d])
  encode (Let xs) = toForeign  ([toForeign "tag"] <> concatMap (\(Tuple k v) -> [toForeign k, encode v]) xs)
  encode (Var s) = toForeign  ["var", s]
  encode (Concat a b xs) = binOpArgs "concat" a b xs
  encode (Downcase a) = unOp "downcase" a
  encode (Upcase a) = unOp "upcase" a
  encode (Rgb r g b) = toForeign  [toForeign "rgb", encode r, encode g, encode b]
  encode (Rgba r g b a) = toForeign  [toForeign "rgba", encode r, encode g, encode b, encode a]
  encode (ToRgba v) = unOp "to-rgba" v
  encode (Minus a b) = binOp "-" a b
  encode (Mult a b) = binOp "*" a b
  encode (Div a b) = binOp "/" a b
  encode (Mod a b) = binOp "%" a b
  encode (Exp a b) = binOp "^" a b
  encode (Plus a b) = binOp "+" a b
  encode (Acos x) = unOp "acos" x
  encode (Asin x) = unOp "asin" x
  encode (Atan x) = unOp "atan" x
  encode (Cos x) = unOp "cos" x
  encode E = constant "e"
  encode (Ln x) = unOp "ln" x
  encode (Ln2 x) = unOp "ln2" x
  encode (Log10 x) = unOp "log10" x
  encode (Log2 x) = unOp "log2" x
  encode (Max a b xs) = binOpArgs "max" a b xs
  encode (Min a b xs) = binOpArgs "min" a b xs
  encode Pi = constant "pi"
  encode (Sin x) = unOp "sin" x
  encode (Sqrt x) = unOp "sqrt" x
  encode (Tan x) =  unOp "tan" x
  encode Zoom = constant "zoom"
  encode HeatmapDensity = constant "heatmap-density"

instance decodeExpression :: Decode Expression where
  decode v = decoded <|> Value <$> decode v
    where
    decoded = decode v >>= \a ->
      case uncons a of
        Just {head,tail} -> do
          tag <- decode head
          go tag tail
        Nothing ->
          fail (ForeignError "empty expression")

    go :: String -> Array Foreign -> F Expression
    go "array" [e] = ArrayExp <$> decode e <*> pure Nothing
    go "array" [ty, e] = ArrayExp <$> decode e <*> (Just <$> (ArrayCheck <$> decode ty <*> pure Nothing))
    go "array" [ty, l, e] =
      ArrayExp <$> decode e <*> (Just <$> (ArrayCheck <$> decode ty <*> (Just <$> decode l)))
    go "boolean" xs = decodeUnOpArgs "boolean" BooleanExp xs
    go "literal" [x] = LiteralExp <$> (Left <$> decode x <|> Right <$> decode x)
    go "number" xs = decodeUnOpArgs "number" NumberExp xs
    go "object" xs = decodeUnOpArgs "object" ObjectExp xs
    go "string" xs = decodeUnOpArgs "string" StringExp xs
    go "to-boolean" xs = decodeUnOp "to-boolean" ToBoolean xs
    go "to-color" xs = decodeUnOpArgs "to-color" ToColor xs
    go "to-number" xs = decodeUnOpArgs "to-number" ToNumber xs
    go "to-string" xs = decodeUnOp "to-string" ToString xs
    go "typeof" xs = decodeUnOp "typeof" TypeOf xs
    go "geometry-type" xs = decodeConstant "geometry-type" GeometryType xs
    go "id" xs = decodeConstant "id" Id xs
    go "properties" xs = decodeConstant "properties" Properties xs
    go "at" [pos, arr] = At <$> decode pos <*> decode arr
    go "get" [prop] = Get <$> decode prop <*> pure Nothing
    go "get" [prop, ob] = Get <$> decode prop <*> (Just <$> decode ob)
    go "has" [prop] = Has <$> decode prop <*> pure Nothing
    go "has" [prop, ob] = Has <$> decode prop <*> (Just <$> decode ob)
    go "length" xs = decodeUnOp "length" Length xs
    go "!" xs = decodeUnOp "!" Not xs
    go "!=" xs = decodeBinOp "!=" NotEq xs
    go "<" xs = decodeBinOp "<" LessThan xs
    go "<=" xs = decodeBinOp "<=" LessThanEq xs
    go "==" xs = decodeBinOp "==" Equal xs
    go ">" xs = decodeBinOp ">" GreaterThan xs
    go ">=" xs = decodeBinOp ">=" GreaterThanEq xs
    go "all" xs = decodeBinOpArgs "all" All xs
    go "any" xs = decodeBinOpArgs "any" Any xs
    go "case" xs
      | Just {init,last} <- unsnoc xs
      , Just cases <- pairs init
      = Case <$> traverse decodeTuple cases <*> decode last
    go "coalesce" xs = decodeBinOpArgs "coalesce" Coalesce xs
    go "match" xs
      | Just {head:input,tail} <- uncons xs
      , Just {init,last:default} <- unsnoc tail
      , Just labelVals <- pairs init
      = Match <$> decode input
              <*> traverse (\(Tuple ls v) -> Tuple <$> decodeLabel ls <*> decode v) labelVals
              <*> decode default
      where
      decodeLabel ls = decodeLabelArray ls <|> decodeLabelSingle ls
      decodeLabelArray ls = decode ls >>= \ls' -> case uncons ls' of
        Just {head,tail} -> pure (NonEmpty head tail)
        Nothing          -> fail (ForeignError "Expected at least on item in label array for match")
      decodeLabelSingle ls = NonEmpty <$> decode ls <*> pure []
    go "interpolate" xs
      | Just {head:t, tail:tail} <- uncons xs
      , Just {head:e, tail:stops'} <- uncons tail
      , Just stops <- pairs stops'
      = Interpolate <$> decode t
                    <*> decode e
                    <*> traverse (\(Tuple i o) -> Tuple <$> decode i <*> decode o) stops
    go "step" xs
      | Just {head:input,tail} <- uncons xs
      , Just {head:out0,tail:tail'} <- uncons tail
      , Just steps <- pairs tail'
      = Step <$> decode input <*> decode out0 <*> traverse decodeTuple steps
    go "let" xs
      | Just bindings <- pairs xs
      = Let <$> traverse decodeTuple bindings
    go "var" [name] = Var <$> decode name
    go "concat" xs = decodeBinOpArgs "concat" Concat xs
    go "donwcase" xs = decodeUnOp "downcase" Downcase xs
    go "upcase" xs = decodeUnOp "upcase" Upcase xs
    go "rgb" [r,g,b] = Rgb <$> decode r <*> decode g <*> decode b
    go "rgba" [r,g,b,a] = Rgba <$> decode r <*> decode g <*> decode b <*> decode a
    go "to-rgba" xs = decodeUnOp "to-rgba" ToRgba xs
    go "-" xs = decodeBinOp "-" Minus xs
    go "*" xs = decodeBinOp "*" Mult xs
    go "/" xs = decodeBinOp "/" Div xs
    go "%" xs = decodeBinOp "%" Mod xs
    go "^" xs = decodeBinOp "^" Exp xs
    go "+" xs = decodeBinOp "+" Plus xs
    go "acos" xs = decodeUnOp "acos" Acos xs
    go "asin" xs = decodeUnOp "asin" Asin xs
    go "atan" xs = decodeUnOp "atan" Atan xs
    go "cos" xs = decodeUnOp "cos" Cos xs
    go "e" xs = decodeConstant "e" E xs
    go "ln" xs = decodeUnOp "ln" Ln xs
    go "ln2" xs = decodeUnOp "ln2" Ln2 xs
    go "log10" xs = decodeUnOp "log10" Log10 xs
    go "log2" xs = decodeUnOp "log2" Log2 xs
    go "max" xs = decodeBinOpArgs "max" Max xs
    go "min" xs = decodeBinOpArgs "min" Min xs
    go "pi" xs = decodeConstant "pi" Pi xs
    go "sin" xs = decodeUnOp "sin" Sin xs
    go "sqrt" xs = decodeUnOp "sqrt" Sqrt xs
    go "tan" xs = decodeUnOp "tan" Tan xs
    go "zoom" xs = decodeConstant "zoom" Zoom xs
    go "heatmap-density" xs = decodeConstant "heatmap-density" HeatmapDensity xs
    
    go unknown _   = fail (ForeignError ("Unknown operand or invalid arguments: " <> unknown))

decodeTuple :: forall a b. Decode a => Decode b => Tuple Foreign Foreign -> F (Tuple a b)
decodeTuple (Tuple c v) = Tuple <$> decode c <*> decode v

constant :: String -> Foreign
constant c = toForeign [c]

decodeConstant :: String -> Expression -> Array Foreign -> F Expression
decodeConstant _   c [] = pure c
decodeConstant tag _ _  = fail (ForeignError ("expected no args for " <> tag))

unOp :: String -> Expression -> Foreign
unOp op e = toForeign [toForeign op, encode e]

decodeUnOp :: String -> (Expression -> Expression) -> Array Foreign -> F Expression
decodeUnOp _   f [x] = f <$> decode x
decodeUnOp tag _ _   = fail (ForeignError ("expected one arg for " <> tag))

unOpArgs :: String -> Expression -> Array Expression -> Foreign
unOpArgs op a xs = toForeign ([toForeign op, encode a] <> map encode xs)

decodeUnOpArgs :: String -> (Expression -> Array Expression -> Expression) -> Array Foreign -> F Expression
decodeUnOpArgs _ f xs
  | Just {head,tail} <- uncons xs = f <$> decode head <*> traverse decode tail
decodeUnOpArgs tag _ _ =
  fail (ForeignError ("expected at least one arg for " <> tag))


binOp :: String -> Expression -> Expression -> Foreign
binOp op a b = toForeign  [toForeign op, encode a, encode b]

decodeBinOp :: String -> (Expression -> Expression -> Expression) -> Array Foreign -> F Expression
decodeBinOp _   f [a,b] = f <$> decode a <*> decode b
decodeBinOp tag _ _   = fail (ForeignError ("expected two args for " <> tag))


binOpArgs :: String -> Expression -> Expression -> Array Expression -> Foreign
binOpArgs op a b xs = toForeign ([toForeign op, encode a, encode b] <> map encode xs)

decodeBinOpArgs :: String -> (Expression -> Expression -> Array Expression -> Expression) -> Array Foreign -> F Expression
decodeBinOpArgs _ f xs
  | Just {head:a, tail} <- uncons xs
  , Just {head:b, tail:rest} <- uncons tail
  = f <$> decode a <*> decode b <*> traverse decode rest
decodeBinOpArgs tag _ _ =
  fail (ForeignError ("expected at least two args for " <> tag))

data InterpolationType
  = Linear
  | Exponential Number
  | CubicBezier
    { x1 :: Number
    , y1 :: Number
    , x2 :: Number
    , y2 :: Number
    }

instance encodeInterpolationType :: Encode InterpolationType where
  encode Linear = toForeign ["linear"]
  encode (Exponential v) = toForeign [toForeign "exponential", toForeign v]
  encode (CubicBezier {x1,y1,x2,y2}) = toForeign [toForeign "cubic-bezier", toForeign x1, toForeign y1, toForeign x2, toForeign y2]
instance decodeInterpolationType :: Decode InterpolationType where
  decode o' = decode o' >>= \o -> case uncons o of
    Just {head, tail} -> do
      tag <- decode head
      case tag of
        "linear"
          | [] <- tail -> pure Linear
          | otherwise  -> fail (ForeignError "expected no arguments to linear interpolation")
        "exponential"
          | [base] <- tail -> Exponential <$> decode base
          | otherwise      -> fail (ForeignError "expected base argument to exponential interpolation")
        "cubic-bezier" -> do
            args <- traverse decode tail
            case args of
              [x1,y1,x2,y2] -> pure (CubicBezier {x1,y1,x2,y2})
              _             -> fail (ForeignError "expected 4 arguments to cubic-bezier interpolation")
        unknown -> fail (ForeignError ("unknown interpolation type: " <> unknown))
    _ -> fail (ForeignError "empty interpolate expression")
                


pairs :: forall a. Array a -> Maybe (Array (Tuple a a))
pairs = pairsImpl Just Nothing Tuple

foreign import pairsImpl
  :: forall a
   . (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> (forall r s. r -> s -> Tuple r s)
  -> Array a
  -> Maybe (Array (Tuple a a))